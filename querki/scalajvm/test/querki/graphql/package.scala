package querki

import org.scalactic.source.Position
import org.scalatest.Matchers._
import play.api.libs.json.{JsArray, JsValue, JsString, JsObject}

package object graphql {

  implicit class RichJsValue(jsv: JsValue) {
    def field(path: String)(implicit p: Position): JsValue =
      (jsv \ path).getOrElse(fail(s"Couldn't find path $path in object $jsv"))

    def obj(path: String)(implicit p: Position): JsObject = {
      field(path) match {
        case o: JsObject => o
        case other => fail(s"Field $path wasn't an Object: $other")
      }
    }

    def string(path: String)(implicit p: Position): String = {
      field(path) match {
        case JsString(s) => s
        case other => fail(s"Field $path wasn't a String: $other")
      }
    }

    def array(path: String)(implicit p: Position): Seq[JsValue] = {
      field(path) match {
        case JsArray(a) => a
        case other => fail(s"Field $path wasn't an Array: $other")
      }
    }

    def name: String = string("_name")
    def hasName(n: String) = name == n
  }

  implicit class RichJsSequence(seq: Seq[JsValue]) {
    def findByName(name: String)(implicit p: Position): JsValue = {
      seq.find(_.hasName(name)).getOrElse(fail(s"Couldn't find an element named $name in $seq"))
    }
  }

}
