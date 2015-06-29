package querki.imexport

import scalatags.Text.short._
import scalatags.generic
import scalatags.text._

import models._
import Thing.PropMap

import querki.core.NameUtils
import querki.core.MOIDs._
import querki.globals._
import querki.types.ModelTypeBase
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
  val typ = "type".t
  val types = "types".t
  val spaceProps = "space-properties".t
  val property = "property".t
  val props = "props".t
  val elem = "e".t
  val model = "model".t
  val models = "models".t
  val instances = "instances".t
  
  val id = "id".a
  val modelref = "model".a
  val name = "name".a
  val coll = "coll".a
  val ptyp = "pType".a
  
  def exportOID(oid:OID) = s"_${oid.toString}"
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
class OIDAttr extends scalatags.Text.GenericAttr[OID]

/**
 * XML Exporter. This is an instance-per-invocation class, with one for each export operation.
 * 
 * @author jducoeur
 */
private [imexport] class XMLExporter(implicit val ecology:Ecology) extends EcologyMember with NameUtils {
  
  lazy val Core = interface[querki.core.Core]
  lazy val PropListManager = interface[querki.core.PropListManager]
  lazy val System = interface[querki.system.System]
  
  lazy val LinkType = Core.LinkType
  
  implicit def PropNameOrdering = PropListManager.PropNameOrdering
  
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
    
    val (mods, insts) = state.allThings.partition(_.isModel)
      
    val complete =
      querki(
        space(
          stdAttrs(state),
          name:=state.name,
          types(allTypes),
          spaceProps(allProperties),
          thingProps(state),
          models(mods.toSeq.map(oneModel)),
          instances(insts.toSeq.map(oneInstance))
        )
      )
    
    val rawStr = """<?xml version="1.0" encoding="UTF-8"?>""" + complete.toString()
    rawStr
  }
  
  def oneType(pt:ModelTypeBase)(implicit state:SpaceState) = {
    typ(
      tid(pt),
      state.anything(pt.basedOn).map { mod =>
        modelref := tname(mod)
      }
    )
  }
  
  def allTypes(implicit state:SpaceState):Seq[Tag] = {
    state.types.values.toSeq.sortBy(_.linkName).map { pt =>
      pt match {
        case mt:ModelTypeBase => Some(oneType(mt))
        case _ => None
      }
    }.flatten
  }
  
  def oneProp(prop:AnyProp)(implicit state:SpaceState):Tag = {
    val propProps = prop.props - CollectionPropOID - TypePropOID - NameOID
    property(
      name:=canonicalize(prop.linkName.get),
      stdAttrs(prop),
      coll:=ref(prop.cType),
      ptyp:=ref(prop.pType),
      thingProps(prop, propProps)
    )
  }
  
  def allProperties(implicit state:SpaceState):Seq[Tag] = {
    state.spaceProps.values.toSeq.sortBy(_.linkName).map { prop =>
      oneProp(prop)
    }
  }
  
  def oneModel(t:Thing)(implicit state:SpaceState):Tag = {
    model(
      stdAttrs(t),
      thingProps(t)
    )
  }
  
  def oneInstance(t:Thing)(implicit state:SpaceState):Tag = {
    Tag(
      t.getModelOpt.map(tname).getOrElse(QuerkiML.exportOID(t.model)),
      List(tid(t) +: Seq(thingProps(t))),
      false
    )
  }
    
  def stdAttrs(t:Thing)(implicit state:SpaceState) = {
    Seq(
      tid(t),
      // We explicitly assume that everything has a model, since the only exception is UrThing:
      modelref:=ref(t.getModelOpt.get)
    )
  }
  
  def thingProps(t:Thing)(implicit state:SpaceState):Tag = thingProps(t, t.props)
  def thingProps(t:PropertyBundle, pm:PropMap)(implicit state:SpaceState):Tag = {
    val propPairs = for {
      pair <- pm
      propId = pair._1
      v = pair._2
      propOpt = state.prop(propId)
      if (propOpt.isDefined)
      prop = propOpt.get
    }
      yield (prop, v)
      
    val sortedProps = propPairs.toSeq.sortBy(_._1)
    
    props(
      sortedProps map { pair =>
        val (prop, qv) = pair
        Tag(
          tname(prop),
          List(propValue(prop, qv)),
          false)
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
          
          case mt:ModelTypeBase => {
            val bundleOpt = elemv.getOpt(mt)
            bundleOpt.map(bundle => thingProps(bundle, bundle.props))
          }
          
          case _ => {
            // We fall back to ordinary serialization, which is fine for most types:
            elemv.pType.serialize(elemv)
          }
        }
      )
    }
  }
  
  def tid(t:Thing) = id := t.id.toThingId
  
  def ref(t:Thing)(implicit state:SpaceState) = {
    t.toThingId
  }
  
  def tname(t:Thing)(implicit state:SpaceState) = {
    t.linkName.map(canonicalize).getOrElse(QuerkiML.exportOID(t.id))
  }
}
