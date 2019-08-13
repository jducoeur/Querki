package querki.graphql

import cats.data.State
import models.{Thing, OID, PType, Property, ThingState}
import querki.core.QLText
import querki.globals.{SpaceState, OID, Ecology}
import sangria.schema
import sangria.schema._
import sangria.validation.ValueCoercionViolation

object OldComputeGraphQL {

  /**
    * This is a pure but extremely deep function, that takes a SpaceState and computes the GraphQL Schema for it.
    *
    * Note that the resulting Schema closes over both the SpaceState and the Ecology. I think that's fine -- they're
    * both effectively immutable -- but keep it in mind.
    */
  def computeGraphQLSchema(state: SpaceState)(implicit ecology: Ecology): Schema[SpaceState, Thing] = {
    implicit val s = state

    lazy val Basic = ecology.api[querki.basic.Basic]
    lazy val Conventions = ecology.api[querki.conventions.Conventions]
    lazy val Core = ecology.api[querki.core.Core]
    lazy val Links = ecology.api[querki.links.Links]

    /////////////////
    //
    // OID Type
    //
    // Since OID totally makes sense as a ScalarType, we define a proper one for that
    //

    case object OIDCoercionViolation extends ValueCoercionViolation("OID Value Expected")

    def parseOID(s: String) = OID.parseOpt(s) match {
      case Some(oid) => Right(oid)
      case None => Left(OIDCoercionViolation)
    }

    val OIDType = ScalarType[OID]("OID",
      coerceOutput = (oid, caps) => oid.toString,
      coerceUserInput = {
        case s: String => parseOID(s)
        case _ => Left(OIDCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) => parseOID(s)
        case _ => Left(OIDCoercionViolation)
      }
    )

    /**
      * Every Thing has an OID field, automatically, because it would be dumb not to.
      */
    val idField = Field[SpaceState, Thing, OID, OID](
      name = "_oid",
      fieldType = OIDType,
      description = Some("The unique OID of this Thing"),
      resolve = ctx => ctx.value.id
    )

    //////////////////////////

    // In principle, I'd like to build these up using a State; in practice, given how recursive the algorithm is,
    // and how constrained it is by the requirements of Sangria, it's much easier to just cheat a little with a
    // couple of local vars.
    // IMPORTANT: if this ever become async, these become extremely suspicious! This is reasonable only because
    // the whole algorithm is single-threaded!
    var objectTypesByModel: Map[OID, ObjectType[SpaceState, Thing]] = Map.empty
    var fieldsByProp: Map[OID, Field[SpaceState, Thing]] = Map.empty

    import querki.core.MOIDs._
    trait PType2Schema[T] {
      type Out = T

      def infoFor(prop: Property[_, _]) = (outputTypeFor(prop), resolverFor(prop))

      // Note that these functions work on OutputType[Any]. That's sad, but given that Querki's types resolve at
      // runtime, there likely isn't much else we can do.
      def outputTypeFor(prop: Property[_, _]): OutputType[Any]
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any]

      def withCollection(prop: Property[_, _])(pt: => OutputType[T]): OutputType[Any] = {
        val inner = pt
        prop.cType.id match {
          case ExactlyOneOID => inner
          case OptionalOID => OptionType(inner)
          case QListOID | QSetOID => ListType(inner)
        }
      }

      def resolve[VT](prop: Property[_, _])(f: VT => T): Context[SpaceState, Thing] => Action[SpaceState, Any] = ctx => {
        val thing = ctx.value
        val result = prop.cType.id match {
          case ExactlyOneOID => f(thing.getFirstOpt(prop).getOrElse(prop.pType.default.elem).asInstanceOf[VT])
          case OptionalOID => thing.getFirstOpt(prop).map(v => f(v.asInstanceOf[VT]))
          case QListOID | QSetOID => thing.getPropAll(prop).map(v => f(v.asInstanceOf[VT]))
        }
        Value(result)
      }
      def stdResolve(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] = {
        resolve(prop)(v => v)
      }
    }

    object Int2Schema extends PType2Schema[Int] {
      def outputTypeFor(prop: Property[_, _]): OutputType[Any] = withCollection(prop)(schema.IntType)
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] = stdResolve(prop)
    }
    object Text2Schema extends PType2Schema[String] {
      def outputTypeFor(prop: Property[_, _]): OutputType[Any] = withCollection(prop)(schema.StringType)
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] =
        resolve[QLText](prop)(_.text)
    }
    object YesNo2Schema extends PType2Schema[Boolean] {
      def outputTypeFor(prop: Property[_, _]): OutputType[Any] = withCollection(prop)(schema.BooleanType)
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] = stdResolve(prop)
    }
    object Name2Schema extends PType2Schema[String] {
      def outputTypeFor(prop: Property[_, _]): OutputType[Any] = withCollection(prop)(schema.StringType)
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] = stdResolve(prop)
    }
    case class Model2Schema(linkModelId: OID) extends PType2Schema[Any] {
      def outputTypeFor(prop: Property[_, _]): OutputType[Any] = withCollection(prop)(modelToObjectType(linkModelId))
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] = ??? // TODO
    }
    object OID2Schema extends PType2Schema[OID] {
      def outputTypeFor(prop: Property[_, _]): OutputType[Any] = withCollection(prop)(OIDType)
      def resolverFor(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, Any] = stdResolve(prop)
    }

    def infoFor(prop: Property[_, _]): (OutputType[Any], Context[SpaceState, Thing] => Action[SpaceState, Any]) = {
      import querki.core.MOIDs._
      val tpe = prop.pType

      val transformer: PType2Schema[_] = tpe.id match {
        case IntTypeOID => Int2Schema
        case TextTypeOID => Text2Schema
        case YesNoTypeOID => YesNo2Schema
        case NameTypeOID => Name2Schema
        case LinkTypeOID => {
          // Link Properties are complicated, since they tend to point to a specific other Model:
          prop.getFirstOpt(Links.LinkModelProp) match {
            case Some(linkModelId) => {
              Model2Schema(linkModelId)
            }
            case None => OID2Schema
          }
        }
      }

      transformer.infoFor(prop)
    }

//    // TODO: this is way too primitive at this point, and doesn't even try to cope with composite types. Make it
//    // much more sophisticated!
//    // Also, think about whether we can make it less horribly coupled. Right now, I'm gathering everything here,
//    // but that's clearly wrong. In principle we want a typeclass, but in Querki's weakly-typed world I suspect
//    // that's simply not an option.
//    def outputTypeFor(prop: Property[_, _]): OutputType[prop.valType] = {
//      import querki.core.MOIDs._
//      val tpe = prop.pType
//
//      val baseType: OutputType[prop.valType] = if (tpe.id == LinkTypeOID) {
//        // Link Properties are complicated, since they tend to point to a specific other Model:
//        prop.getFirstOpt(Links.LinkModelProp) match {
//          case Some(linkModelId) => {
//            modelToObjectType(linkModelId).asInstanceOf[OutputType[prop.valType]]
//          }
//          case None => OIDType.asInstanceOf[OutputType[prop.valType]]
//        }
//      } else {
//        tpe.id match {
//          // This is hideous, and really wants Scala 3 Match Types, but for now...
//          case IntTypeOID => schema.IntType.asInstanceOf[OutputType[prop.valType]]
//          case TextTypeOID => schema.StringType.asInstanceOf[OutputType[prop.valType]]
//          case YesNoTypeOID => schema.BooleanType.asInstanceOf[OutputType[prop.valType]]
//          case NameTypeOID => schema.StringType.asInstanceOf[OutputType[prop.valType]]
//          // TODO: more types...
//        }
//      }
//    }
//
//      def fieldInfoFor(prop: Property[_, _]): (OutputType[prop.valType], Context[SpaceState, Thing] => Action[SpaceState, prop.valType]) = {
//        import querki.core.MOIDs._
//        val tpe = prop.pType
//
//        val (baseType, fetcher) = if (tpe.id == LinkTypeOID) {
//          // Link Properties are complicated, since they tend to point to a specific other Model:
//          prop.getFirstOpt(Links.LinkModelProp) match {
//            case Some(linkModelId) => {
//              (modelToObjectType(linkModelId),
//                )
//            }
//            case None => OIDType
//          }
//        } else {
//          tpe.id match {
//            // This is hideous, and really wants Scala 3 Match Types, but for now...
//            case IntTypeOID => schema.IntType
//            case TextTypeOID => schema.StringType
//            case YesNoTypeOID => schema.BooleanType
//            case NameTypeOID => schema.StringType
//            // TODO: more types...
//          }
//        }
//
//        prop.cType.id match {
//          case ExactlyOneOID => baseType
//          // This is hideous, and really wants Scala 3 Match Types, but for now...
//          case OptionalOID => OptionType(baseType).asInstanceOf[OutputType[prop.valType]]
//          case QListOID | QSetOID => ListType(baseType).asInstanceOf[OutputType[prop.valType]]
//        }
//      }
//
//    def resolverForProp(prop: Property[_, _]): Context[SpaceState, Thing] => Action[SpaceState, prop.valType] = ctx => {
//      val thing: Thing = ctx.value
//
//      ???
//    }

    def propToField(propId: OID): Field[SpaceState, Thing] = {
      fieldsByProp.get(propId) match {
        case Some(field) => field
        case None => {
          val field: Field[SpaceState, Thing] = state.prop(propId) match {
            case Some(prop) => {
              val (outputType, resolver) = infoFor(prop)
              Field[SpaceState, Thing, Any, Any](
                name = prop.linkName.getOrElse(propId.toString),
                fieldType = outputType,
                description = prop.getFirstOpt(Conventions.PropSummary).map(_.text),
                resolve = resolver
              )
            }
            case None => {
              // Missing prop, so snip it off:
              Field[SpaceState, Thing, Boolean, Boolean](s"UnknownProp$propId", schema.BooleanType, None, resolve = _ => Value(false))
            }
          }
          fieldsByProp += (propId -> field)
          field
        }
      }
    }

    def modelToFields(model: Thing): List[Field[SpaceState, Thing]] = {
      // TODO: add ID as a Field:
      idField +: model.props.keys.map(propToField).toList
    }

    def modelToObjectType(id: OID): ObjectType[SpaceState, Thing] = {
      objectTypesByModel.get(id) match {
        case Some(objectType) => objectType
        case None => {
          val objectType: ObjectType[SpaceState, Thing] = state.anything(id) match {
            case Some(model) => {
              // We need to use the indirect constructor for ObjectType, or we're going to hit infinite recursion:
              ObjectType(model.linkName.getOrElse(id.toString), () => modelToFields(model))
            }
            case None => {
              // Bad pointer, so snip it off:
              ObjectType(s"UnknownModel$id", List.empty)
            }
          }
          objectTypesByModel += (id -> objectType)
          objectType
        }
      }
    }

    // Populate all of the Models:
    val models = state.allModels
    // Note that this is side-effecting, because it's all very recursive between the ObjectTypes and Fields. It
    // should populate both of the Maps at the top.
    models.foreach(model => modelToObjectType(model.id))

    val OIDArg = Argument("oid", OIDType, "the OID of the desired Instance")

    val Query = ObjectType[SpaceState, Thing](
      "Query",
      fields[SpaceState, Thing](
        objectTypesByModel.toList.map { case (modelId, objectType) =>
          Field[SpaceState, Thing, Thing, Thing](
            // Uncapitalize the name, since that seems to be the convention:
            objectType.name.head.toLower + objectType.name.drop(1),
            objectType,
            arguments = OIDArg :: Nil,
            // TODO: deal with the OID not being found:
            resolve = ctx => Value(ctx.ctx.anything(ctx arg OIDArg).get)
          )
        }:_*
      )
    )

    Schema(Query)
  }
}
