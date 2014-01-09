package models.system

import models._

import Thing._

import OIDs._

import SystemSpace._

import querki.basic
import querki.ecology._
import querki.values._

/**
 * The Ur-Thing, from which the entire world descends. Note that this is
 * its own parent!
 */
object UrThing extends ThingState(RootOID, systemOID, RootOID,
    toProps(
      setName("Thing"),
      // TODO: once we rework the UI some more, we probably can and should remove this Optional from here.
      // It is really only here to remind the Space author to think about whether something is a Model.
      (querki.core.MOIDs.IsModelOID -> ExactlyOne(ElemValue(false, new DelegatingType(YesNoType))))
      ))(querki.ecology.Ecology)
{
  override def getProp(propId:OID)(implicit state:SpaceState):PropAndVal[_] = {
    // If we've gotten up to here and haven't found the property, use
    // the default:
    localOrDefault(propId)
  }
    
  override def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):QValue = {
    localPropVal(prop).getOrElse(prop.default)
  }
  
  override def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
    props.contains(propId)
  }
    
  override def allProps(implicit state:SpaceState):Set[Property[_,_]] = localProps
  
  override def isAncestor(other:OID)(implicit state:SpaceState):Boolean = false
  
  override def hasModel = false
}

