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
  
  /**
   * Deal with "unwrapping" a DelegatingType, to make get() work:
   */
  def realType(declaringType:PType[_]):PType[_] = {
    if (declaringType.isInstanceOf[models.DelegatingType[_]])
      declaringType.asInstanceOf[models.DelegatingType[_]].realType
    else
      declaringType    
  }
  
  lazy val myType = realType(pType)
  
  def get[VT](expectedType:PType[VT]):VT = {
    val realExpected = realType(expectedType)
    if (realExpected == myType)
      // Here is The One Great Evil Cast, checked as best we can at runtime. Let's see if we can eliminate
      // all others.
      elem.asInstanceOf[VT]
    else {
      try {
        throw new Exception("Trying to cast ElemValue " + elem + ", of type " + myType + " to " + realExpected)
      } catch {
        case e:Exception => play.api.Logger.error("", e); throw e
      }
    }
  }
  
  def getOpt[VT](expectedType:PType[VT]):Option[VT] = {
    val realExpected = realType(expectedType)
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