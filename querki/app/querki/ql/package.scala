package querki

import querki.ecology._

import querki.values.{ElemValue, QValue}

package object ql {

  /**
   * This trait should be used by any type that can consider itself "code" -- in particular, that wants to be
   * displayable in the _code() method. 
   */
  trait CodeType {
    def code(elem:ElemValue):String
  }
  
  trait QL extends EcologyInterface {
    def WarningValue(msg:String):QValue
    def ErrorValue(msg:String):QValue
    
    def EmptyListCut():QValue
  }

}