package querki

import models._

import querki.core.PropList
import querki.ecology._
import querki.values.{QValue, SpaceState}

package object types {
  object MOIDs extends EcotIds(10) {
    // Old Things, moved to here
    val MinTextLengthOID = sysId(100)
    
    val MinIntValueOID = moid(1)
    val MaxIntValueOID = moid(2)
    
    val WrappedValueTypeOID = moid(3)
    val DefaultValuePropOID = moid(4)
    val ModelForTypePropOID = moid(5)
    val AppliesToTypesPropOID = moid(6)
    
    val WithPropertyOID = moid(7)
  }
  
  implicit def vals2Bundle(vals:(OID, QValue)*):SimplePropertyBundle = SimplePropertyBundle(vals:_*)
  
  /**
   * This is the "public face" of a Model Type, with the interesting stuff about it.
   */
  trait ModelTypeBase extends PType[ModeledPropertyBundle] with PTypeBuilder[ModeledPropertyBundle, SimplePropertyBundle]  {
    def basedOn:OID
    def copy(newModelId:OID, newProps:PropMap):ModelTypeBase
  }
  
  trait Types extends EcologyInterface {
    /**
     * An internal Type that can be wrapped around *any* value. Currently has no UI, so use with caution.
     */
    def WrappedValueType:PType[QValue] with PTypeBuilder[QValue,QValue]
    
    /**
     * The ModelForTypeProp is the pointer from a Model Type to the actual Model that it is wrapped around.
     * This is generally only needed for save/load and other type-construction operations.
     */
    def ModelForTypeProp:Property[OID,OID]
    
    /**
     * The DefaultValueProp is pretty much what it says: it is the default value for this Property. That
     * is, it is what you get when you hard-query for a Property on a Thing (not an Opt query), and that
     * Property is *not* defined on the Thing.
     * 
     * Note that DefaultValueProp is a full-on QValue. That's because its Type is, in principle, unknown.
     * It should match the Type and Collection of the Property it is being applied to, though, if you
     * want to avoid strange and confusing behaviour.
     * 
     * In principle, it would be nice to expose this to end users to use. In practice, that's going to
     * be challenging from a UI perspective: you have to feed in the expected Type and Collection to produce
     * the correct input control. It's doable in principle, but enough work that I'm not going to bother
     * until we care.
     */
    def DefaultValueProp:Property[QValue,QValue]    

    /**
     * This meta-meta-Property should be placed on user-visible meta-Properties that are relevant for a
     * particular Type. It is mainly intended for Editing, but lives here to be closer to the center of
     * the onion. 
     */
    def AppliesToTypesProp:Property[OID,OID]
  
    def MinTextLengthProp:Property[Int, Int]
    def MinIntValueProp:Property[Int, Int]
    def MaxIntValueProp:Property[Int, Int]
    
    def rebuildBundle(existingOpt:Option[PropertyBundle], containers:List[IndexedOID], innerV:FormFieldInfo)(implicit state:SpaceState):Option[FormFieldInfo]
  }
  
  object DeriveNameMOIDs extends EcotIds(12) {
    val DeriveNameOID = moid(1)
    val DeriveModelOID = moid(2)
    val DeriveAlwaysOID = moid(3)
    val DeriveInitiallyOID = moid(4)
    val DeriveNeverOID = moid(5)
  }
  
  trait DeriveName extends EcologyInterface {
    def DeriveNameProp:Property[OID,OID]
    
    def DeriveAlways:Thing
    def DeriveInitially:Thing
    def DeriveNever:Thing
    
    def filterNameIfDerived(state:SpaceState, model:Thing, props:PropList, propPair:(Property[_,_], DisplayPropVal)):Boolean
    def nameIsDerived(thing:Thing, state:SpaceState):Boolean
  }
  
  /**
   * This Ecot manages the concept of "paths" to Properties. Use it on the relatively rare occasions where you
   * are trying to figure out all the different ways to get *to* a particular Property, including via Model Types.
   * 
   * See the PropPath trait for more details.
   */
  trait PropPaths extends EcologyInterface {
    /**
     * Get all of the paths for accessing this Property in this Space. You then usually use getPropOpt() on
     * those Paths to test whether specific Things can access it.
     * 
     * Note that this is a pretty intensive call; it is usually best to call it *before* your tight loops if possible.
     */
    def pathsToProperty[VT](prop:Property[VT,_])(implicit state:SpaceState):Seq[PropPath[VT,_]]
  }
}
