package models.system

import models._

import Thing._

import OIDs._

import SystemSpace._

/**
 * The Ur-Thing, from which the entire world descends. Note that this is
 * its own parent!
 */
object UrThing extends ThingState(RootOID, systemOID, RootOID,
    toProps(
      setName("Thing"),
      (IsModelOID -> PropValue(Some(ElemValue(false))))
      )) 
{
  override def getProp(propId:OID)(implicit state:SpaceState):PropAndVal[_,_] = {
    // If we've gotten up to here and haven't found the property, use
    // the default:
    localOrDefault(propId)
  }
    
  override def getPropVal[VT, CT](prop:Property[VT, _, CT])(implicit state:SpaceState):PropValue[CT] = {
    localPropVal(prop).getOrElse(prop.default)
  }
  
  override def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
    props.contains(propId)
  }
    
  override def allProps(implicit state:SpaceState):Set[Property[_,_,_]] = localProps
  
  override def isAncestor(other:OID)(implicit state:SpaceState):Boolean = false
}
  

object Page extends ThingState(PageOID, systemOID, RootOID,
    toProps(
      setName("Simple-Page"),
      IsModelProp(true),
      DisplayTextProp("""
This is the basic Page Thing. Use it as your Model for *basic* Pages without real structure.
            
Use the **DisplayText** property to indicate what to show on the page. You can put anything in there.
""")))

object SimpleThing extends ThingState(SimpleThingOID, systemOID, RootOID,
    toProps(
      setName("Simple-Thing"),
      IsModelProp(true)))

object PhotoBase extends ThingState(PhotoBaseOID, systemOID, RootOID,
    toProps(
      setName("Photograph-Base"),
      IsModelProp(true),
      DisplayTextProp("""
This is the Model for all uploaded photographs. You shouldn't try to base something on this directly --
just upload a photograph, and you'll get one of these.
""")),
    Kind.Attachment
    )