package querki

import models.Property
import models.system.OIDs.sysId

import querki.ecology._
import querki.values.QValue

import modules.ModuleIds

package object types {
  object MOIDs extends ModuleIds(10) {
    // Old Things, moved to here
    val MinTextLengthOID = sysId(100)
    
    val MinIntValueOID = moid(1)
    val MaxIntValueOID = moid(2)
    
    val WrappedValueTypeOID = moid(3)
    
    val DefaultValuePropOID = moid(4)
  }
  
  trait Types extends EcologyInterface {
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
  
    def MinTextLengthProp:Property[Int, Int]
    def MinIntValueProp:Property[Int, Int]
    def MaxIntValueProp:Property[Int, Int]
  }
}
