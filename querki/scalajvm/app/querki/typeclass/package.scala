package querki

import querki.core.MethodDefs
import querki.ecology._
import querki.globals._

package object typeclass {

  trait Typeclasses extends EcologyInterface {
    def WhoMethod: MethodDefs#AbstractFunction
    def DateMethod: MethodDefs#AbstractFunction
    def ThingMethod: MethodDefs#AbstractFunction
  }
}
