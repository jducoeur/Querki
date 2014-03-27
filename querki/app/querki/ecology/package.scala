package querki

import akka.actor.{ActorRef, Props}

import scala.reflect.runtime.universe.TypeTag

package object ecology {
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:InterfaceWrapper[T]):T = {
    wrapper.get
  }
  
  type CreateActorFunc = (Props, String) => Option[ActorRef]
}
