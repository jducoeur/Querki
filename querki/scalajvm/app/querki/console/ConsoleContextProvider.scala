package querki.console

import querki.api._
import querki.globals._
import querki.identity.User

/**
 * Typeclass representing a way to get access to the context of a Console
 * command. In practice, T is generally an AutowireApiImpl.
 */
trait ConsoleContextProvider[T] {
  /**
   * Who is issuing this command?
   */
  def user:User
  /**
   * Iff this command pertains to a Space, the relevant SpaceState.
   */
  def stateOpt:Option[SpaceState]
}

object ConsoleContextProvider {
  implicit def plainContextProvider(impl:AutowireApiImpl) = new ConsoleContextProvider[AutowireApiImpl] {
    def user = impl.user
    def stateOpt = None
  }
  
  implicit def stateContextProvider(impl:SpaceApiImpl):ConsoleContextProvider[SpaceApiImpl] = new ConsoleContextProvider[SpaceApiImpl] {
    def user = impl.user
    def stateOpt = Some(impl.state)
  }
}
