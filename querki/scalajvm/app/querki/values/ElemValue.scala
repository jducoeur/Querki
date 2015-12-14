package querki.values

import models.PType

import querki.globals._

/**
 * The value of a primitive Type. These are always considered "elements", since they
 * are always wrapped inside Collections.
 * 
 * Note that ElemValue is untyped. The is necessary -- if we try to keep this strongly
 * typed, then the matrix composition of Collections and PTypes becomes impossible at
 * runtime. So ElemValues are fundamentally not typesafe, and should only be evaluated
 * in the context of their associated PTypes.
 */
case class ElemValue(elem:Any, pType:PType[_]) {
  import ElemValue._
  
  lazy val myType = pType.realType
  
  def getBase[VT, RT](expectedType:PType[VT])(succ:VT => RT, fail: => RT):RT = {
    if (matchesTypeExact(this, expectedType))
      // Here is The One Great Evil Cast, checked as best we can at runtime. Let's see if we can eliminate
      // all others.
      succ(elem.asInstanceOf[VT])
    else if (myType.canCoerceTo(expectedType.realType)) {
      // We're not the correct type, but we can get there:
      myType.coerceTo(expectedType.realType, this).getBase(expectedType)(succ, fail)
    } else {
      fail
    }    
  }
  
  def get[VT](expectedType:PType[VT]):VT = {
    getBase(expectedType)(
      { v => v },
      {
        try {
          throw new Exception("Trying to cast ElemValue " + elem + ", of type " + myType + " to " + expectedType.realType)
        } catch {
          case e:Exception => play.api.Logger.error("", e); throw e
        }
     })
  }
  
  def getOpt[VT](expectedType:PType[VT]):Option[VT] = {
    getBase(expectedType)(
      { v => Some(v) },
      { None }
    )
  }
  
  override def toString = elem.toString
}

object ElemValue {
  def matchesTypeExact(left:PType[_], right:PType[_]):Boolean = {
    left.realType == right.realType
  }
  
  def matchesTypeExact(elem:ElemValue, pt:PType[_]):Boolean = {
    matchesTypeExact(elem.pType, pt)
  }
  
  def matchesType(left:PType[_], right:PType[_]):Boolean = {
    matchesTypeExact(left, right) || (left.canCoerceTo(right))
  }
}
