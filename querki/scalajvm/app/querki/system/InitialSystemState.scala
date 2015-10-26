package querki.system

import models.{Collection, OIDMap, Property, PType, ThingState}
import models.Thing.emptyProps

import querki.ecology._

import querki.values.SpaceState

object InitialSystemState {
  /**
   * This creates the initial, completely-empty System Space. You then create the desired Ecots to fill this in.
   */
  def create(ecology:Ecology) = {
    // To start things off, we create the System Space with *no* Properties, because we haven't actually created the
    // Properties yet! We'll add them in later, at the end of system initialization in SystemEcot:
    SpaceState(SystemIds.systemOID, querki.core.MOIDs.RootOID, emptyProps,
      querki.identity.MOIDs.SystemUserOID, "System", querki.time.epoch, Seq.empty, None, 
      OIDMap[PType[_]](), OIDMap[Property[_,_]](), OIDMap[ThingState](), OIDMap[Collection](), 
      None)    
  }
}