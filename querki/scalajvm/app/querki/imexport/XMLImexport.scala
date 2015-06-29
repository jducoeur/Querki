package querki.imexport

import scalatags.Text.short._
import scalatags.generic
import scalatags.text._

import models._
import Thing.PropMap

import querki.core.NameUtils
import querki.core.MOIDs._
import querki.globals._
import querki.values.QValue

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
  val spaceProps = "space-properties".t
  val property = "property".t
  val props = "props".t
  val elem = "e".t
  
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
private [imexport] class XMLExporter(implicit val ecology:Ecology) extends EcologyMember with NameUtils {
  
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  lazy val LinkType = Core.LinkType
  
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
          spaceProps(allProperties),
          thingProps(state)
        )
      )
    
    val rawStr = """<?xml version="1.0" encoding="UTF-8"?>""" + complete.toString()
    rawStr
  }
  
  def oneProp(prop:AnyProp)(implicit state:SpaceState):Tag = {
    val propProps = prop.props - CollectionPropOID - TypePropOID - NameOID
    property(
      name:=canonicalize(prop.linkName.get),
      stdAttrs(prop),
      coll:=ref(prop.cType),
      typ:=ref(prop.pType),
      thingProps(prop, propProps)
    )
  }
  
  def allProperties(implicit state:SpaceState):Seq[Tag] = {
    state.spaceProps.values.toSeq.sortBy(_.linkName).map { prop =>
      oneProp(prop)
    }
  }
    
  def stdAttrs(t:Thing)(implicit state:SpaceState) = {
    Seq(
      id := t.id.toThingId,
      // We explicitly assume that everything has a model, since the only exception is UrThing:
      model:=ref(t.getModelOpt.get)
    )
  }
  
  def thingProps(t:Thing)(implicit state:SpaceState):Tag = thingProps(t, t.props)
  def thingProps(t:Thing, pm:PropMap)(implicit state:SpaceState):Tag = {
    props(
      pm.toSeq.map { pair =>
        val (propId, qv) = pair
        state.prop(propId) match {
          case Some(prop) => {
            Tag(
              tname(prop),
              List(propValue(prop, qv)),
              false)
          }
          case None => Tag("MISSING_${propId.toString}", List.empty, true)
        }
      }
    )
  }
  
  def propValue(prop:AnyProp, qv:QValue)(implicit state:SpaceState):Seq[Tag] = {
    qv.cv.toSeq.map { elemv =>
      elem(
        elemv.pType match {
          case LinkType => {
            val topt:Option[Thing] = for {
              oid <- elemv.getOpt(LinkType)
              thing <- state.anything(oid)
            }
              yield thing
              
            val result:String = topt.map(tname(_)).getOrElse(s"MISSING THING ${elemv.elem.toString()}")
            result
          }
          case _ => {
            // We fall back to ordinary serialization, which is fine for most types:
            elemv.pType.serialize(elemv)
          }
        }
      )
    }
  }
  
  def ref(t:Thing)(implicit state:SpaceState) = {
    t.toThingId
  }
  
  def tname(t:Thing)(implicit state:SpaceState) = {
    t.linkName.map(canonicalize).getOrElse(s"_${t.id.toString}")
  }
}
