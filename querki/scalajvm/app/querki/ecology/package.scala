package querki

import akka.actor.{ActorRef, Props}

import querki.values.SpaceState

package object ecology {
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:InterfaceWrapperBase[SpaceState, EcotImpl, T]):T = {
    wrapper.get
  }

  type Ecology = EcologyBase[SpaceState, EcotImpl]
  
  type EcologyMember = EcologyMemberBase[SpaceState, EcotImpl]
  
  type Ecot = EcotBase[SpaceState, EcotImpl]
  
  type CreateActorFunc = (Props, String) => Option[ActorRef]
}
