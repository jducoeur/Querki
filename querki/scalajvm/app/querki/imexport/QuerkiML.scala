package querki.imexport

import scalatags.Text.short._
import scalatags.generic
import scalatags.text._

import models.{AsName, AsOID, OID, ThingId}

import querki.core.NameUtils

/**
 * We build the export using Scalatags, because hey -- if it's a better way to write HTML,
 * it's a better way to write XML in general.
 */
private [imexport] object QuerkiML extends scalatags.generic.Util[Builder, String, String]
  with scalatags.Text.Cap
  with scalatags.DataConverters
{
  // We introduce our own indirection here, mainly so that we can enhance this with a parser
  // a bit down the road:
  implicit class QUtil(str:String) {
    def t = str.tag[String]
    def a = str.attr
  }
  
  type Tag = scalatags.Text.TypedTag[String]
  type Attr = scalatags.Text.Attr
  
  val querki = "querki".t
  val space = "space".t
  val typ = "type".t
  val types = "types".t
  val spaceProps = "space-properties".t
  val property = "property".t
  val props = "props".t
  val elem = "e".t
  val model = "model".t
  val models = "models".t
  val instances = "instances".t
  
  val namespace = "xmlns".a
  
  val id = "id".a
  val modelref = "model".a
  val name = "name".a
  val coll = "coll".a
  val ptyp = "pType".a
  
  def exportOID(oid:OID) = s"_!${oid.toString}"
  def importOIDOpt(str:String):Option[OID] = if (str.startsWith("_!")) Some(OID(str.drop(2))) else None
  def importOID(str:String):OID = importOIDOpt(str).getOrElse(throw new Exception(s"Expecting OID, got $str"))
}

class ThingAttr extends scalatags.Text.AttrValue[ThingId] {
  def apply(t:Builder, a:Attr, v:ThingId) {
    val str = v match {
      case AsOID(oid) => QuerkiML.exportOID(oid)
      case AsName(name) => NameUtils.canonicalize(name)
    }
    t.setAttr(a.name, str)
  }  
}
class AsOIDAttr extends scalatags.Text.AttrValue[AsOID] {
  def apply(t:Builder, a:Attr, v:AsOID) {
    t.setAttr(a.name, QuerkiML.exportOID(v.oid))
  }
}
class OIDAttr extends scalatags.Text.AttrValue[OID] {
  def apply(t:Builder, a:Attr, v:OID) {
    t.setAttr(a.name, QuerkiML.exportOID(v))
  }  
}
