package querki.graphql

import cats.data._
import cats.data.Validated._
import cats.implicits._
import cats.effect.IO

import sangria.ast.Field

import querki.basic.PlainText
import querki.core.QLText
import querki.globals._
import models.{Thing, DisplayText, OID, Wikitext}

import play.api.libs.json.{JsValue, JsString, JsNumber}

/**
  * Typeclass that represents the notion of a type that can be converted to a JsValue.
  *
  * @tparam VT the VType of a Querki PType
  */
trait JsValueable[VT] {
  def toJsValue(v: VT, field: Field, thing: Thing): Res[JsValue]
}

/**
  * Instances of JsValueable.
  *
  * This is structured as a mixin with FPComputeGraphQL. They're really bound at the hip, but we cake-pattern them
  * just to reduce the size of that file a bit.
  */
trait JsValueableMixin { self: FPComputeGraphQL =>
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

  /**
    * Another serious exception: Functions result in a JsArray of whatever type comes out of the function.
    * Depending on that result, you may or may not be able to drill down into them.
    */
  val functionJsValueable = new JsValueable[QLText] {
    def toJsValue(v: QLText, field: Field, thing: Thing) = processQL(v, thing, field)
  }

  implicit class JsValueableOps[T: JsValueable](t: T) {
    def toJsValue(field: Field, thing: Thing): Res[JsValue] = {
      implicitly[JsValueable[T]].toJsValue(t, field, thing: Thing)
    }
  }
}
