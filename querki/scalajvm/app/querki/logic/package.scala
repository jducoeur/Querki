package querki

import models.PType

import querki.ecology._
import querki.globals._
import querki.ql.Invocation
import querki.values.{ElemValue, QFut, QValue}

package object logic {
  trait Logic extends EcologyInterface {
    /**
     * This is the master concept of comparison between two QValues. It deals with such messy problems as
     * DelegatingType and Type Coercion, validates that the two values are the same size, and applies the
     * comparer stepwise to each pair of elements. Returns true iff the comparer was true in all cases.
     */
    def compareValues(firstIn:QValue, secondIn:QValue)(comparer:(PType[_], ElemValue, ElemValue) => Boolean):Boolean
    
    def PlusMethod:querki.core.MethodDefs#AbstractFunction
    
    def True:ElemValue
    def False:ElemValue
  }
}