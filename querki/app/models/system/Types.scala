package models.system

import scala.xml._

import play.api.Logger
import play.api.templates.Html

import models._

import Thing._

import OIDs._

import ql._

import querki.ecology._
import querki.types.Types
import querki.util._
import querki.values._

trait CommonInputRenderers { self:SystemType[_] =>
  
  def renderAnyText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_])(doRender: (String) => Elem):Elem = {
    val str = elemT.toUser(v)
    val xml = doRender(str)
    xml
  }
  
  def renderLargeText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_]):Elem = {
    renderAnyText(prop, state, currentValue, v, elemT) { cv =>
      <textarea class="_largeTextEdit" rows="2">{cv}</textarea>
    }
  }
  
  def renderText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_]):Elem = {
    renderAnyText(prop, state, currentValue, v, elemT) { cv =>
      <input type="text" value={cv}/>
    }
  }
  
  def renderBlank(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):Elem = {
    renderText(prop, state, currentValue, Core.TextType(""), Core.TextType)
  }
}

abstract class SystemType[T](tid:OID, pf:PropFetcher)(implicit e:Ecology = querki.ecology.theEcology) 
  extends PType[T](tid, systemOID, querki.core.MOIDs.RootOID, pf)(e) with CommonInputRenderers
{
  lazy val Types = interface[Types]
  
  def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
    // TBD: this is smelly -- the fact that we need to know here how to render Optional is a nasty abstraction
    // break. But in general, rendering probably doesn't belong here: ultimately, rendering depends on the
    // Collection/Type matrix, and there doesn't seem to be a nice clean division of responsibilities...
    val renderedBlank = for (
      ev <- currentValue.effectiveV;
      if (displayEmptyAsBlank && ev.cType == Core.Optional && ev.isEmpty)
        )
      yield renderBlank(prop, state, currentValue, this)
      
    renderedBlank.getOrElse(renderText(prop, state, currentValue, v, this))
  }
  
  // Iff a Type wants to render QNone as blank text instead of the default value, set this to true
  val displayEmptyAsBlank:Boolean = false
}

object SystemTypes {
  def all = OIDMap[PType[_]]()  
}