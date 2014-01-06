package querki

import models.{Property, Thing}
import models.system.PlainText
import models.system.OIDs.sysId

import querki.ecology._

/**
 * This Module contains "Basic" Querki -- some Properties and such that aren't core (not central to the
 * operations of the system), but which all users will probably want to make use of.
 */
package object basic {
  object MOIDs extends EcotIds(17) {
    val PageOID = sysId(8)
    val SimpleThingOID = sysId(23)
    val DisplayNameOID = sysId(26)
    val PhotoBaseOID = sysId(30)
    val BulletedOID = sysId(56)
    val DisplayThingTreeOID = sysId(66)
    val AllThingsOID = sysId(67)
    val CommasMethodOID = sysId(80)
    val AllPropsThingOID = sysId(84)
  }
  
  trait Basic extends EcologyInterface {  
    def DisplayNameProp:Property[PlainText,String]
    def SimpleThing:Thing
  }
}