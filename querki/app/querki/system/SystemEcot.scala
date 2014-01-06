package querki.system

import querki.ecology._
import querki.values.SpaceState

/**
 * The interface to manage the System.
 */
trait SystemManagement extends EcologyInterface {
  /**
   * Set the final System Space. The code that initialized the Ecology *must* call this once complete!
   */
  def setState(state:SpaceState)
}

object SystemMOIDs extends EcotIds(18)

class SystemEcot(e:Ecology) extends QuerkiEcot(e) with System with SystemManagement {
  def setState(state:SpaceState) = {
    _state = Some(state)
  }
  
  var _state:Option[SpaceState] = None
  def State = _state.getOrElse(throw new Exception("Attempting to access the System Space before init is complete!"))
}