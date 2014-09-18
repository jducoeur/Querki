package querki

package object ecology {
  /**
   * For now, this is just a placeholder, in case the client wants to do any init-time state.
   */
  case class ClientState()
  
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:InterfaceWrapperBase[ClientState, EcotImpl, T]):T = {
    wrapper.get
  }

  type Ecology = EcologyBase[ClientState, EcotImpl]
  
  type EcologyMember = EcologyMemberBase[ClientState, EcotImpl]
  
  type Ecot = EcotBase[ClientState, EcotImpl]
  
  trait EcotImpl extends Ecot
  
  type EcologyImpl = EcologyImplBase[ClientState, EcotImpl]
}
