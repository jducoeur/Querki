package models

import language.existentials

import querki.util._
import querki.values._

/**
 * This class is the result of parsing a field out of a Play HTTP Form. It crosses the
 * HTTP/internal API lines somewhat uncomfortably; not clear yet where it belongs.
 */
case class FormFieldInfo(prop:Property[_,_], value:Option[QValue], empty:Boolean, isValid:Boolean, raw:Option[String] = None, error:Option[PublicException] = None) {
  def propId = prop.id
  def isEmpty = empty || value.isEmpty
  
  override def toString = {
    s"""FormFieldInfo -- Property ${prop.displayName} (${prop.id}):
       |    empty: $empty; isValid: $isValid
       |    ${value.getOrElse("None")}""".stripMargin
  }
}
