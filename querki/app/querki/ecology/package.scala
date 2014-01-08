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
  var Ecology:Ecology = null
  
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:InterfaceWrapper[T]):T = {
    wrapper.get
  }
}
