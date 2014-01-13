package querki.core

import scala.xml.Elem

import models.{DisplayPropVal, OID, Property, PType}
import models.Thing.PropFetcher

import querki.ecology._
import querki.values.{ElemValue, SpaceState}

private[core] trait BootUtils { self:CoreModule =>
  def setInternal:(OID, QValue)
}

object TypeUtils {
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

  abstract class SystemType[T](tid:OID, pf:PropFetcher)(implicit e:Ecology) 
    extends PType[T](tid, SystemIds.systemOID, querki.core.MOIDs.RootOID, pf)(e) with CommonInputRenderers
  {
    // Types is where the various validators and such live:
    lazy val Types = interface[querki.types.Types]
    
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
}
