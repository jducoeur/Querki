package querki.graphql

import cats.data._
import cats.data.Validated._
import cats.effect.IO
import cats.implicits._
import models.{Thing, OID, ThingId, DisplayText, PType, Property, Wikitext}
import play.api.libs.json._
import querki.basic.PlainText
import querki.basic.MOIDs._
import querki.core.MOIDs._
import querki.tags.MOIDs._
import querki.core.QLText
import querki.globals._
import querki.values.{PropAndVal, RequestContext, QValue}
import sangria.ast._
import sangria.parser.QueryParser

import scala.util.{Success, Failure}

class FPComputeGraphQL(implicit rc: RequestContext, state: SpaceState, ecology: Ecology) {
  final val thingQueryName = "_thing"
  final val instancesQueryName = "_instances"
  final val idArgName = "_oid"
  final val nameArgName = "_name"

  type SyncRes[T] = ValidatedNec[GraphQLError, T]
  type Res[T] = EitherT[IO, NonEmptyChain[GraphQLError], T]

  lazy val Basic = ecology.api[querki.basic.Basic]
  lazy val Core = ecology.api[querki.core.Core]
  lazy val QL = ecology.api[querki.ql.QL]
  lazy val Tags = ecology.api[querki.tags.Tags]

  // This particular typeclass instance needs to live in here, because it continues to dive down into the stack:

  trait JsValueable[VT] {
    def toJsValue(v: VT, field: Field, thing: Thing): Res[JsValue]
  }

  object JsValueable {
    implicit val intJsValueable = new JsValueable[Int] {
      def toJsValue(v: Int, field: Field, thing: Thing) = res(JsNumber(v))
    }

    implicit val textJsValueable = new JsValueable[QLText] {
      def toJsValue(v: QLText, field: Field, thing: Thing): Res[JsValue] = {
        // TODO: make getArgumentEnum smarter, so that it only accepts values of a specific Enumeration:
        val args: SyncRes[(Boolean, String)] =
          (field.getArgumentBoolean("render", true), field.getArgumentEnum("mode", "STRIP")).mapN(Tuple2.apply)
        args match {
          case Valid((true, mode)) => {
            def renderFromMode(wikitext: Wikitext): DisplayText = {
              mode match {
                case "RAW" => wikitext.raw
                case "STRIP" => wikitext.strip
                case "HTML" => wikitext.display
              }
            }

            // Render the QL...
            val thingContext = thing.thisAsContext
            EitherT.right(IO.fromFuture(IO {
              val wikitextFuture = QL.process(v, thingContext, lexicalThing = Some(thing))
              val textFuture: Future[DisplayText] =
                wikitextFuture.map(renderFromMode(_))
              textFuture.map(displayText => JsString(displayText.toString))
            }))
          }
          case Valid((false, _)) => {
            // We're not rendering, so return the exact text:
            res(JsString(v.text))
          }
          case Invalid(err) => EitherT.leftT(err)
        }
      }
    }
    implicit val plainTextJsValueable = new JsValueable[PlainText] {
      def toJsValue(v: PlainText, field: Field, thing: Thing) = res(JsString(v.text))
    }

    implicit val linkJsValueable = new JsValueable[OID] {
      def toJsValue(v: OID, field: Field, thing: Thing) = {
        // This is the really interesting one. This is a link, so we recurse down into it:
        state.anything(v) match {
          case Some(thing) => processThing(thing, field)
          case None => resError(OIDNotFound(v.toThingId.toString, field.location))
        }
      }
    }

    /**
      * Tags are a weird neither-fish-nor-fowl, and they violate the GraphQL convention that things need to be
      * rigidly typed. This gets evaluated differently depending on how it is called. If there are sub-selections,
      * we try to dereference it; if not, we just return the tag's text.
      */
    val tagJsValueable = new JsValueable[PlainText] {
      def toJsValue(v: PlainText, field: Field, thing: Thing) = {
        if (field.selections.isEmpty) {
          // Treat the tag as simple text:
          res(JsString(v.text))
        } else {
          // There are sub-selections, so dereference it:
          val name = v.text
          val thing = state.anythingByName(name) match {
            case Some(thing) => thing
            case None => state.anythingByDisplayName(name) match {
              case Some(thing) => thing
              case None => Tags.getTag(name, state)
            }
          }
          processThing(thing, field)
        }
      }
    }

    implicit class JsValueableOps[T: JsValueable](t: T) {
      def toJsValue(field: Field, thing: Thing): Res[JsValue] = {
        implicitly[JsValueable[T]].toJsValue(t, field, thing: Thing)
      }
    }
  }
  import JsValueable._

  def handle(query: String): IO[JsValue] = {
    val result: IO[Either[NonEmptyChain[GraphQLError], JsValue]] = processQuery(query).value
    result.map {
      _ match {
        case Right(jsv) => JsObject(Map("data" -> jsv))
        case Left(errs) => {
          val jsErrs: NonEmptyChain[JsValue] = errs.map { err =>
            val msg = err.msg
            err.location.map { loc =>
              val jsLoc = JsObject(Map(
                "line" -> JsNumber(loc.line),
                "column" -> JsNumber(loc.column)
              ))
              JsObject(Map(
                "message" -> JsString(msg),
                "locations" -> JsArray(Seq(jsLoc))
              ))
            }.getOrElse {
              JsObject(Map("message" -> JsString(msg)))
            }
          }
          JsObject(Map(
            "data" -> JsNull,
            "errors" -> JsArray(jsErrs.toNonEmptyList.toList)
          ))
        }
      }
    }
  }

  def processQuery(query: String): Res[JsValue] = {
    val fields: SyncRes[Vector[Field]] =
      parseQuery(query) andThen
        (getDefinitions(_)) andThen
        (_.map(confirmIsQuery(_)).nonEmptySequence) andThen
        (singleOperation(_)) andThen
        (_.selections.map(confirmIsField).sequence)

    fields match {
      case Valid(sels) => {
        val queryResults: Vector[Res[(String, JsValue)]] = sels.map(processQuerySelection(_))
        val inverted: Res[Vector[(String, JsValue)]] = queryResults.sequence
        // Surely we can do this better by defining a Semigroup over JsObject:
        inverted.map { pairs =>
          pairs.foldLeft(JsObject(Seq.empty)) { case (obj, (name, jsv)) =>
            obj + (name -> jsv)
          }
        }
      }
      case Invalid(err) => EitherT.leftT(err)
    }
  }

  def parseQuery(query: String): SyncRes[Document] = {
    QueryParser.parse(query) match {
      case Success(document) => document.validNec
      case Failure(ex) => ParseFailure(ex.getMessage).invalidNec
    }
  }

  def getDefinitions(doc: Document): SyncRes[NonEmptyList[Definition]] = {
    NonEmptyList.fromList(doc.definitions.toList) match {
      case Some(defs) => defs.valid
      case None => NoDefinitions.invalidNec
    }
  }

  def confirmIsQuery(definition: Definition): SyncRes[OperationDefinition] = {
    definition match {
      case op: OperationDefinition if (op.operationType == OperationType.Query) => op.valid
        // TODO: deal with other Operation types!
      case op: OperationDefinition => UnhandledOperationType(op.operationType, definition.location).invalidNec
        // TODO: deal with other Definition types!
      case _ => UnhandledDefinitionType(definition, definition.location).invalidNec
    }
  }

  def confirmIsField(selection: Selection): SyncRes[Field] = {
    selection match {
      case field: Field => field.valid
      case _ => UnhandledSelectionType(selection, selection.location).invalidNec
    }
  }

  def singleOperation(ops: NonEmptyList[OperationDefinition]): SyncRes[OperationDefinition] = {
    if (ops.length > 1)
      TooManyOperations.invalidNec
    else
      ops.head.valid
  }

  /**
    * Processes a top-level Query selection.
    *
    * These are somewhat different, since they follow certain required rules. Lower-level selections are starting
    * from an existing set of Things, so they have a lot more flexibility.
    *
    * This is an IO, because we can have Futures down underneath in the processing.
    *
    * This returns a name/value pair. The name is the name of the top-level selection; the value is the contents
    * of what we found for that selection.
    */
  def processQuerySelection(field: Field): Res[(String, JsValue)] = {
    if (field.name == thingQueryName) {
      // This is a specific-thing query
      withThingFromArgument(field, thingQueryName) { thing =>
        processThing(thing, field)
      }
    } else if (field.name == instancesQueryName) {
      // This is querying all instances of a Model
      withThingFromArgument(field, instancesQueryName) { model =>
        val things = state.descendants(model.id, includeModels = false, includeInstances = true).toList
        val processedThings: Res[List[JsValue]] = things.map { thing =>
          processThing(thing, field)
        }.sequence
        processedThings.map(JsArray(_))
      }
    } else {
      resError(IllegalTopSelection(field.name, field.location))
    }
  }

  /**
    * We expect this Field to have either an OID or Name argument, specifying a Thing. Find that Thing, run the
    * provided processing function, and tuple the result with the selectionName for the resulting JSON.
    */
  def withThingFromArgument(field: Field, selectionName: String)(f: Thing => Res[JsValue]): Res[(String, JsValue)] = {
    // If an alias was specified, we use that to return the result:
    val returnName = field.alias.getOrElse(selectionName)
    for {
      thing <- getThingFromArgument(field, selectionName).toRes
      jsValue <- f(thing)
    }
      yield (returnName, jsValue)
  }

  /**
    * This field should have an argument specifying a Thing. Find that Thing.
    */
  def getThingFromArgument(field: Field, selectionName: String): SyncRes[Thing] = {
    field.getArgumentStr(idArgName, "") match {
      case Valid(oidStr) if (!oidStr.isEmpty) => {
        OID.parseOpt(oidStr) match {
          case Some(oid) => {
            state.anything(oid) match {
              case Some(thing) => thing.valid
              case None => OIDNotFound(oidStr, field.location).invalidNec
            }
          }
          case None => NotAnOID(oidStr, field.location).invalidNec
        }
      }
      case Valid(_) => {
        field.getArgumentStr(nameArgName, "") match {
          case Valid(thingName) if (!thingName.isEmpty) => {
            state.anythingByName(thingName) match {
              case Some(thing) => thing.valid
              case None => NameNotFound(thingName, field.location).invalidNec
            }
          }
          case Valid(_) => MissingRequiredArgument(field, selectionName, s"$idArgName or $nameArgName", field.location).invalidNec
          case Invalid(err) => err.invalid
        }
      }
      case Invalid(err) => err.invalid
    }
  }

  /**
    * The given Field specifies a Thing. Given that Thing, process its selections, and return the JsValue of the
    * results.
    */
  def processThing(thing: Thing, parentField: Field): Res[JsValue] = {
    val selectionResults: Res[Vector[(String, JsValue)]] = parentField.selections.map { selection =>
      for {
        childField <- confirmIsField(selection).toRes
        jsValue <- processField(thing, childField)
      }
        yield (childField.name, jsValue)
    }.sequence
    selectionResults.map { pairs =>
      val pairMap = pairs.toMap
      JsObject(pairMap)
    }
  }

  /**
    * Given a Thing, process one Field that represents a Property of that Thing.
    */
  def processField(thing: Thing, field: Field): Res[JsValue] = {
    if (field.name == idArgName) {
      // They're asking for the OID of this Thing:
      res(JsString(thing.id.toThingId.toString))
    } else if (field.name == nameArgName) {
      res(JsString(thing.toThingId.toString))
    } else {
      getProperty(thing, field).toRes.flatMap { prop =>
        processProperty(thing, prop, field)
      }
    }
  }

  def getProperty(thing: Thing, field: Field): SyncRes[Property[_, _]] = {
    val name = field.name
    val propOpt = for {
      // Since neither spaces nor dashes are legal in GraphQL field names, we have to use underscore:
      thingId <- ThingId.parseOpt(name.replace('_', '-'))
      thing <- state.anything(thingId)
      prop <- asProp(thing)
    }
      yield prop

    propOpt.syncOrError(UnknownProperty(name, field.location))
  }

  def processProperty(thing: Thing, prop: Property[_, _], field: Field): Res[JsValue] = {
    // TODO: this is ugly and hardcoded -- can we make it better? Do note that we need to rehydrate the types here
    // *somehow*, so this isn't trivial:
    prop.pType.id match {
      case IntTypeOID => processTypedProperty(thing, prop.confirmType(Core.IntType), Core.IntType, field)
      case TextTypeOID => processTypedProperty(thing, prop.confirmType(Core.TextType), Core.TextType, field)
      case LargeTextTypeOID => processTypedProperty(thing, prop.confirmType(Core.LargeTextType), Core.LargeTextType, field)
      case LinkTypeOID => processTypedProperty(thing, prop.confirmType(Core.LinkType), Core.LinkType, field)
      case PlainTextOID => processTypedProperty(thing, prop.confirmType(Basic.PlainTextType), Basic.PlainTextType, field)
      case NewTagSetOID => processTypedProperty(thing, prop.confirmType(Tags.NewTagSetType), Tags.NewTagSetType, field)(tagJsValueable)
      case _ => resError(UnsupportedType(prop, field.location))
    }
  }

  def processTypedProperty[VT](thing: Thing, propOpt: Option[Property[VT, _]], pt: PType[VT], field: Field)(implicit ev: JsValueable[VT]): Res[JsValue] = {
    val resultOpt: Option[Res[JsValue]] = propOpt.map { prop =>
      thing.getPropOpt(prop) match {
        case Some(propAndVal) => processValues(thing, prop, propAndVal.rawList, field)
        case None => {
          if (prop.id == DisplayNameOID) {
            // Display Name is a very special case. It's specifically non-inherited, so it's entirely plausible that
            // getPropOpt() might show it as entirely not existing. But it's really common, so we don't want to
            // error on it. So we instead call a spade a spade:
            res(JsString(""))
          } else {
            resError(PropertyNotOnThing(thing, prop, field.location))
          }
        }
      }
    }
    resultOpt.getOrElse(resError(InternalGraphQLError("Hit a Property whose type doesn't confirm!", field.location)))
  }

  def processValues[VT: JsValueable](thing: Thing, prop: Property[VT, _], vs: List[VT], field: Field): Res[JsValue] = {
    // What we return depends on the Collection of this Property:
    prop.cType.id match {
      case ExactlyOneOID => {
        if (vs.isEmpty) {
          resError(MissingRequiredValue(thing, prop, field.location))
        } else {
          vs.head.toJsValue(field, thing)
        }
      }
      case OptionalOID => {
        vs.headOption match {
          case Some(v) => v.toJsValue(field, thing)
            // TODO: check the GraphQL spec -- is JsNull correct here?
          case None => res(JsNull)
        }
      }
      case QListOID | QSetOID => {
        val jsvs: Res[List[JsValue]] = vs.map(_.toJsValue(field, thing)).sequence
        jsvs.map(JsArray(_))
      }
      case other => {
        // We don't expect this to happen until and unless we open up the possibility of more Collections:
        QLog.error(s"FPComputeGraphQL: request to process a collection of type ${prop.cType} for Property ${prop.displayName}")
        res(JsNull)
      }
    }
  }

  def res[T](v: T): Res[T] = {
    EitherT.rightT(v)
  }
  def resError[T](err: => GraphQLError): Res[T] = {
    EitherT.leftT(NonEmptyChain(err))
  }

  def asProp(thing: Thing): Option[Property[_, _]] = {
    thing match {
      case t: Property[_, _] => Some(t)
      case _ => None
    }
  }

  implicit class RichOption[T](tOpt: Option[T]) {
    def syncOrError(err: => GraphQLError): SyncRes[T] = {
      tOpt match {
        case Some(t) => t.valid
        case None => err.invalidNec
      }
    }
  }

  trait GraphQLValue[T <: Value, R] {
    def name: String
    def fromValue(v: Value): Option[T]
    def value(t: T): R
  }

  implicit val BooleanV = new GraphQLValue[BooleanValue, Boolean] {
    val name = "Boolean"
    def fromValue(v: Value): Option[BooleanValue] = {
      v match {
        case b: BooleanValue => Some(b)
        case _ => None
      }
    }
    def value(t: BooleanValue): Boolean = t.value
  }

  implicit val StringV = new GraphQLValue[StringValue, String] {
    val name = "String"
    def fromValue(v: Value): Option[StringValue] = {
      v match {
        case b: StringValue => Some(b)
        case _ => None
      }
    }
    def value(t: StringValue): String = t.value
  }

  implicit val EnumV = new GraphQLValue[EnumValue, String] {
    val name = "Enum"
    def fromValue(v: Value): Option[EnumValue] = {
      v match {
        case b: EnumValue => Some(b)
        case _ => None
      }
    }
    def value(t: EnumValue): String = t.value
  }

  implicit class RichField(field: Field) {
    def getArgument[T <: Value, R](name: String, default: => R)(implicit graphQLValue: GraphQLValue[T, R]): SyncRes[R] = {
      field.arguments.find(_.name == name) match {
        case Some(arg) => {
          graphQLValue.fromValue(arg.value) match {
            case Some(v) => graphQLValue.value(v).valid
            case None => UnexpectedArgumentType(arg, "Boolean", field.location).invalidNec
          }
        }
        case None => default.valid
      }
    }

    def getArgumentBoolean(name: String, default: => Boolean): SyncRes[Boolean] =
      getArgument[BooleanValue, Boolean](name, default)
    def getArgumentStr(name: String, default: => String): SyncRes[String] =
      getArgument[StringValue, String](name, default)
    def getArgumentEnum(name: String, default: => String): SyncRes[String] =
      getArgument[EnumValue, String](name, default)
  }

  implicit class RichSyncRes[T](syncRes: SyncRes[T]) {
    def toRes: Res[T] = {
      syncRes match {
        case Valid(t) => EitherT.rightT(t)
        case Invalid(e) => EitherT.leftT(e)
      }
    }
  }
}

sealed trait GraphQLError {
  def msg: String
  def location: Option[AstLocation]
}
case class ParseFailure(msg: String) extends GraphQLError {
  val location = None
}
case object NoDefinitions extends GraphQLError {
  val msg = "No Definitions provided in the GraphQL Document!"
  val location = None
}
case class UnhandledOperationType(opType: OperationType, location: Option[AstLocation]) extends GraphQLError {
  val msg = s"Querki does not yet deal with ${opType.getClass.getSimpleName} operations; sorry."
}
case class UnhandledDefinitionType(definition: Definition, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"Querki can not yet deal with ${definition.getClass.getSimpleName}"
}
case object TooManyOperations extends GraphQLError {
  val msg = "Querki can currently only deal with one Query at a time; sorry."
  val location = None
}
case class UnhandledSelectionType(selection: Selection, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"Querki can not yet deal with ${selection.getClass.getSimpleName} selections; sorry."
}
case class UnexpectedArgumentType(arg: Argument, expected: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"${arg.value.getClass.getSimpleName} requires an argument of type $expected."
}
case class MissingRequiredArgument(field: Field, name: String, tpe: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"Field ${field.name} requires a $tpe argument named '$name'"
}
case class UnknownThing(name: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"There is no Thing named $name"
}
case class IllegalTopSelection(name: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"The top level of a GraphQL query must be _thing or _instances."
}
case class NotAnOID(str: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"$str is not a valid OID"
}
case class OIDNotFound(oid: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"No Thing found with OID $oid"
}
case class NameNotFound(name: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"No Thing found named $name -- maybe that isn't the correct Link Name?"
}
case class UnknownProperty(name: String, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"Unknown Property: $name"
}
case class PropertyNotOnThing(thing: Thing, prop: AnyProp, location: Option[AstLocation]) extends GraphQLError {
  def msg = s"${thing.displayName} does not have requested property ${prop.displayName}"
}
case class UnsupportedType(prop: Property[_, _], location: Option[AstLocation]) extends GraphQLError {
  def msg = s"Querki GraphQL does not yet support ${prop.pType.displayName} properties; sorry."
}
case class InternalGraphQLError(msg: String, location: Option[AstLocation]) extends GraphQLError
case class MissingRequiredValue(thing: Thing, prop: Property[_, _], location: Option[AstLocation]) extends GraphQLError {
  def msg = s"Required Property ${prop.displayName} on Thing ${thing.displayName} is empty!"
}