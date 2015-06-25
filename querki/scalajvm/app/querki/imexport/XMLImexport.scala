package querki.imexport

import scalatags.Text.short._
import scalatags.generic
import scalatags.text._

import models.{MIMEType, Thing, ThingId}

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
  
  val id = "id".a
  val model = "model".a
}
  
class ThingAttr extends scalatags.Text.GenericAttr[ThingId]
class OIDAttr extends scalatags.Text.GenericAttr[OID]

/**
 * XML Exporter. This is an instance-per-invocation class, with one for each export operation.
 * 
 * @author jducoeur
 */
private [imexport] class XMLExporter(implicit val ecology:Ecology) extends EcologyMember {
  
  lazy val System = interface[querki.system.System]
  
  import QuerkiML._
  implicit def tidAttr = new ThingAttr
  implicit def oidAttr = new OIDAttr
  
  var prefixMap = Map.empty[SpaceState, String]
  
  def exportSpace(state:SpaceState):String = {
    implicit val s = state
    prefixMap += (System.State -> "ss")
    prefixMap += (state -> "s1")
      
    val complete =
      querki(
        space(
          stdAttrs(state)
        )
      )
    
    complete.toString()
//    ExportedContentImpl(complete.toString().getBytes, state.displayName, MIMEType.XML)
  }
  
  def prefix(implicit state:SpaceState) = prefixMap(state)
    
  def stdAttrs(t:Thing)(implicit state:SpaceState) = {
    Seq(id := t.id, model := t.model)
  }
}
