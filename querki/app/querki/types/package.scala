package querki

import models.PropertyInterface
import models.system.OIDs.sysId

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
  
  val DefaultValueProp = new PropertyInterface[QValue,QValue]
}
