package querki

import models.PType

import querki.ecology._
import querki.globals._
import querki.ql.Invocation
import querki.values.{ElemValue, QFut, QValue}

package object logic {
  trait Logic extends EcologyInterface {
    def PlusMethod:querki.core.MethodDefs#AbstractFunction
    def MinusMethod: querki.core.MethodDefs#AbstractFunction
    
    def True:ElemValue
    def False:ElemValue
  }
}
