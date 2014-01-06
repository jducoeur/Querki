package querki.system

import querki.ecology._
import querki.values.SpaceState

private[system] trait SystemManagement extends EcologyInterface {
  def setState(state:SpaceState)
}

object SystemMOIDs extends EcotIds(18)

class SystemEcot(e:Ecology) extends QuerkiEcot(e) with System with SystemManagement {
  def setState(state:SpaceState) = {
    models.system.SystemSpace._state = Some(state)
  }
  
  def SystemState = models.system.SystemSpace.State
}