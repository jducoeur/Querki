package querki

import querki.ecology._
import querki.values.{ElemValue}

package object logic {

  trait Logic extends EcologyInterface {
    def PlusMethod: querki.core.MethodDefs#AbstractFunction
    def MinusMethod: querki.core.MethodDefs#AbstractFunction

    def True: ElemValue
    def False: ElemValue
  }
}
