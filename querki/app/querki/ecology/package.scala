package querki

import scala.reflect.runtime.universe.TypeTag

package object ecology {
  /**
   * The One True Ecology.
   * 
   * IMPORTANT: Note that, while there is a single static copy of the Ecology for now, and you are *allowed* to use
   * that for the time being, you are *not* encouraged to do so. The static Ecology hampers testing:
   * in particular, since mocking out parts of the Ecology is essential for testing, having a static
   * pointer here essentially prevents us from doing testing in parallel in the same memory space. So
   * you should pass the Ecology trait around as a pointer whenever possible. In the long run, we will
   * likely require that in all cases. 
   */
  val Ecology = new EcologyImpl
  
  /**
   * Shorthand for fetching a specified EcologyInterface. Note that the interface must be registered and
   * initialized, or this will throw an exception!
   * 
   * This should only be used by non-Ecot code, and should be minimized when possible -- this sort of static
   * access to the Ecology isn't optimal in the long run. Ecot code should use the interface() method that is
   * defined there. (Therefore, use of getInterface is the main "bad smell" that tells us where the Ecology is
   * being accessed statically.)
   */
  def getInterface[T <: EcologyInterface : TypeTag]:T = Ecology.api[T]
  
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:InterfaceWrapper[T]):T = {
    wrapper.get
  }
}
