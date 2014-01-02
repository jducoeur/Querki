package querki

import models.PropertyInterface

import models.system.QLText
import models.system.OIDs.sysId

import modules.ModuleIds

/**
 * Querki's "core" objects
 * 
 * This is conceptually the center of the onion, at least in terms of static declarations of
 * actual Things. querki.core contains the Things that are *frequently* used elsewhere. As such,
 * it can be depended upon by all other Modules, and doesn't depend on any of them.
 */
package object core {
  object MOIDs extends ModuleIds(16) {
    val UrPropOID = sysId(5)
  }
  
  val UrProp = new PropertyInterface[QLText, String]
}
