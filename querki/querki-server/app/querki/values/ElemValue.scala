package querki.values

import models.PType

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
  
  lazy val myType = pType.realType
  
  def matchesType[VT](expectedType:PType[VT]):Boolean = {
    val realExpected = expectedType.realType
    realExpected == myType
  }
  
  def get[VT](expectedType:PType[VT]):VT = {    
    if (matchesType(expectedType))
      // Here is The One Great Evil Cast, checked as best we can at runtime. Let's see if we can eliminate
      // all others.
      elem.asInstanceOf[VT]
    else {
      try {
        throw new Exception("Trying to cast ElemValue " + elem + ", of type " + myType + " to " + expectedType.realType)
      } catch {
        case e:Exception => play.api.Logger.error("", e); throw e
      }
    }
  }
  
  def getOpt[VT](expectedType:PType[VT]):Option[VT] = {
    val realExpected = expectedType.realType
    if (realExpected == myType)
      // Here is The One Great Evil Cast, checked as best we can at runtime. Let's see if we can eliminate
      // all others.
      Some(elem.asInstanceOf[VT])
    else {
      None
    }
  }
  
  override def toString = elem.toString
}