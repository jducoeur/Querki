package querki.imexport

import play.api.libs.json._

import models._
import querki.core.NameUtils
import querki.globals._
import querki.values.QValue

class JsonExport(implicit val state: SpaceState, val ecology: Ecology) extends EcologyMember {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val Models = interface[models.Models]
  
  lazy val PropValType = Models.PropValType
  
  lazy val ExactlyOne = Core.ExactlyOne
  lazy val Optional = Core.Optional
  lazy val QList = Core.QList
  lazy val QSet = Core.QSet
  
  implicit val plaintextWrites = new Writes[querki.basic.PlainText] {
    def writes(text: querki.basic.PlainText) = Json.toJson(text.text)
  }
  
  implicit val qltextWrites = new Writes[querki.core.QLText] {
    def writes(text: querki.core.QLText) = Json.toJson(text.text)
  }
  
  // A PropMap get recursed into:
  implicit val propMapWrites = new Writes[PropMap] {
    def writes(propMap: PropMap) = {
      val underlying: Map[String, JsValue] = propMap.map { case (propId, qv) =>
        val prop = state.prop(propId).get
        val multi: Boolean = prop.cType match {
          case ExactlyOne => false
          case Optional => false
          case QList => true
          case QSet => true
        }
        (NameUtils.canonicalize(prop.linkName.get), jsonifyInternal(qv, multi))
      }
      new JsObject(underlying)
    }
  }
  
  /**
   * Given a particular PType, a QValue and whether we expect that QValue to be multi-valued, this
   * jsonifies that QValue if it is of that PType.
   * 
   * The "multi" parameter is to give the system at least a little predictability, so that the reader of
   * this JSON can reasonably expect to get a JSON array if and only if the Property if multi-valued,
   * regardless of the number of elements actually in it.
   */
  def jsonifyIf[VT : Writes](pt: PType[VT])(implicit qv: QValue, multi: Boolean): Option[JsValue] = {
    if (qv.pType == pt) {
      if (multi)
        Some(Json.toJson(qv.rawList(pt)))
      else
        qv.firstAs(pt) match {
          case Some(v) => Some(Json.toJson(v))
          case None => Some(JsNull)
        }
    } else
      None
  }
  
  /**
   * Just a little syntactic sugar for jsonifyInternal.
   */
  private implicit class RichOption(l: Option[JsValue]) {
    def or(r: => Option[JsValue]): Option[JsValue] = {
      l match {
        case Some(jsv) => Some(jsv)
        case None => r
      }
    }
  }
  
  def jsonifyInternal(implicit qv: QValue, multi: Boolean): JsValue = {
    val resultOpt = 
      jsonifyIf(PropValType) or
      jsonifyIf(Core.TagType) or
      jsonifyIf(Core.TextType) or
      jsonifyIf(Core.LargeTextType) or
      jsonifyIf(Basic.PlainTextType) or
      jsonifyIf(Core.IntType)
    
    resultOpt.getOrElse(throw new Exception(s"Don't yet know how to JSONify a value of Type ${qv.pType.displayName}!"))
  }
  
  /**
   * Given an arbitrary QV, this turns it into JSON.
   * 
   * This will eventually work recursively through tree of links, but we need to make sure that we
   * can catch loops before we do that. We should have a firm maximum depth allowed.
   */
  def jsonify(qv: QValue): String = {
    Json.stringify(jsonifyInternal(qv, true))
  }
}
