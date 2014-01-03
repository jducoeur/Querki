package querki

import models.PropertyInterface
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
    
  }
  
  val DefaultValueProp = new PropertyInterface[QValue,QValue]
  val MinTextLengthProp = new PropertyInterface[Int, Int]
  val MinIntValueProp = new PropertyInterface[Int, Int]
  val MaxIntValueProp = new PropertyInterface[Int, Int]
}
