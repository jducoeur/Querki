package querki.console

import querki.api._
import querki.globals._
import querki.identity.User
import querki.values.RequestContext

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
  /**
   * The RC of this invocation.
   */
  def rc:RequestContext
}

object ConsoleContextProvider {
  implicit def plainContextProvider(impl:AutowireApiImpl) = new ConsoleContextProvider[AutowireApiImpl] {
    def user = impl.user
    def stateOpt = None
    def rc = impl.rc
  }
  
  implicit def stateContextProvider(impl:SpaceApiImpl):ConsoleContextProvider[SpaceApiImpl] = new ConsoleContextProvider[SpaceApiImpl] {
    def user = impl.user
    def stateOpt = Some(impl.state)
    def rc = impl.rc
  }
}
