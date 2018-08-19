package querki

import models.{AnyProp, Property, Thing}

import querki.ecology._
import querki.globals._
import querki.values.QValue

package object datamodel {
  
  val DataModelTag = "Data and Spaces"
  
  trait DataModelAccess extends EcologyInterface {
    def IsFunctionProp:Property[Boolean,Boolean]
    def CopyIntoInstances:Property[Boolean,Boolean]
    
    /**
     * Signal Value, to indicate a Value that has been intentionally removed.
     */
    def getDeletedValue(prop:AnyProp)(implicit state:SpaceState):QValue
    
    def isDeletable(t:Thing, allowIfProp:Boolean = false)(implicit state:SpaceState):Boolean
  }
  
  trait Choices extends EcologyInterface {
    def ChooseFromPropProp: Property[OID, OID]
    def ChooseFromThingProp: Property[OID, OID]
  }
}
