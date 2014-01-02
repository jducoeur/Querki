package querki

import modules.ModuleIds
import models.system.OIDs.{sysId}

package object conventions {
  object MOIDs extends ModuleIds(15) {
    // Old OIDs, moved to here:
    val PropSummaryOID = sysId(85)
    val PropDetailsOID = sysId(86)
    
    val PropDescriptionOID = moid(1)    
  }
  
  val PropSummary = modules.Modules.Conventions.PropSummary
  val PropDetails = modules.Modules.Conventions.PropDetails
}