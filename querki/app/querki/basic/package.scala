package querki

import models.{Property, Thing}
import models.system.PlainText
import models.system.OIDs.sysId

import querki.ecology._

import modules.ModuleIds

/**
 * This Module contains "Basic" Querki -- some Properties and such that aren't core (not central to the
 * operations of the system), but which all users will probably want to make use of.
 */
package object basic {
  object MOIDs extends ModuleIds(17) {
    val PageOID = sysId(8)
    val SimpleThingOID = sysId(23)
    val DisplayNameOID = sysId(26)
  }
  
  trait Basic extends EcologyInterface {  
    def DisplayNameProp:Property[PlainText,String]
    def SimpleThing:Thing
  }
}