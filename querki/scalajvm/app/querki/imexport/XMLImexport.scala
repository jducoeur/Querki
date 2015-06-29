package querki.imexport

import scalatags.Text.short._
import scalatags.generic
import scalatags.text._

import models._

import querki.globals._

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
  
  val querki = "querki".t
  val space = "space".t
  val property = "property".t
  
  val id = "id".a
  val model = "model".a
  val name = "name".a
  val coll = "coll".a
  val typ = "type".a
}

class ThingAttr extends scalatags.Text.GenericAttr[ThingId]
class AsOIDAttr extends scalatags.Text.GenericAttr[AsOID]
class OIDAttr extends scalatags.Text.GenericAttr[OID]

/**
 * XML Exporter. This is an instance-per-invocation class, with one for each export operation.
 * 
 * @author jducoeur
 */
private [imexport] class XMLExporter(implicit val ecology:Ecology) extends EcologyMember {
  
  lazy val System = interface[querki.system.System]
  
  type Tag = scalatags.Text.TypedTag[String]
  
  import QuerkiML._
  implicit def tidAttr = new ThingAttr
  implicit def asOIDAttr = new AsOIDAttr
  implicit def oidAttr = new OIDAttr
  
  var prefixMap = Map.empty[SpaceState, String]
  
  def exportSpace(state:SpaceState):String = {
    implicit val s = state
    prefixMap += (System.State -> "ss")
    prefixMap += (state -> "s1")
      
    val complete =
      querki(
        space(
          stdAttrs(state),
          name:=state.name,
          allProperties
        )
      )
    
    val rawStr = """<?xml version="1.0" encoding="UTF-8"?>""" + complete.toString()
    rawStr
  }
  
  def oneProp(prop:AnyProp)(implicit state:SpaceState):Tag = {
    property(
      stdAttrs(prop),
      coll:=ref(prop.cType),
      typ:=ref(prop.pType)
    )
  }
  
  def allProperties(implicit state:SpaceState):Seq[Tag] = {
    state.spaceProps.values.toSeq.map { prop =>
      oneProp(prop)
    }
  }
    
  def stdAttrs(t:Thing)(implicit state:SpaceState) = {
    Seq(id := t.id.toThingId, model := t.model)
  }
  
  def ref(t:Thing)(implicit state:SpaceState) = {
    t.toThingId
  }
}
