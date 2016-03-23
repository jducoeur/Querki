package querki

import models.PType

import querki.ecology._
import querki.globals._
import querki.ql.Invocation
import querki.values.{ElemValue, QFut, QValue}

package object logic {
  
  /**
   * Mix-in for a PType that provides a definition of _add.
   * 
   * TODO: this is a nasty hack. Figure out how we're actually going to deal with generic function
   * signatures and specializations. This is basically the camel's nose in the tent of QL-level typeclasses.
   */
  trait AddableType {
    /**
     * This should be a specialized definition of _add for this PType.
     */
    def qlApplyAdd(inv:Invocation):QFut
  }
  
  trait Logic extends EcologyInterface {
    /**
     * This is the master concept of comparison between two QValues. It deals with such messy problems as
     * DelegatingType and Type Coercion, validates that the two values are the same size, and applies the
     * comparer stepwise to each pair of elements. Returns true iff the comparer was true in all cases.
     */
    def compareValues(firstIn:QValue, secondIn:QValue)(comparer:(PType[_], ElemValue, ElemValue) => Boolean):Boolean
    
    def True:ElemValue
    def False:ElemValue
  }
}