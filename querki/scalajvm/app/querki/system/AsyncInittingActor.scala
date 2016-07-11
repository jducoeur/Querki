package querki.system

import akka.actor._

import querki.globals._

/**
 * Trait that any critical node-singleton Actors should implement. Adds the
 * initted() method, which should be called when this Actor is ready to go,
 * and which tells QuerkiRoot that it is initted.
 * 
 * TBD: is there a way to do this that doesn't require f-bounds?
 */
trait AsyncInittingActor[T <: AsyncInittingActor[T]] extends querki.ecology.AsyncInitter { self:Actor with EcologyMember =>
  def initted()(implicit tag:scala.reflect.ClassTag[T]) = {
    val clazz = tag.runtimeClass
    val systemManagement = interface[querki.system.SystemManagement]
    systemManagement.asyncInitTarget ! AsyncInitted(clazz)
  }
}

case class AsyncInitted(clazz:Class[_])
