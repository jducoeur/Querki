package querki

import models.PropertyInterface
import models.system.PlainText
import models.system.OIDs.sysId

import modules.ModuleIds

/**
 * This Module contains "Basic" Querki -- some Properties and such that aren't core (not central to the
 * operations of the system), but which all users will probably want to make use of.
 */
package object basic {
  object MOIDs extends ModuleIds(17) {
    val DisplayNameOID = sysId(26)
  }
  
  val DisplayNameProp = new PropertyInterface[PlainText,String]
}