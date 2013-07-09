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
  lazy val myType = {
    if (pType.isInstanceOf[models.DelegatingType[_]])
      pType.asInstanceOf[models.DelegatingType[_]].realType
    else
      pType
  }
  
  def get[VT](expectedType:PType[VT]):VT = {
    if (expectedType == myType)
      // Here is The One Great Evil Cast, checked as best we can at runtime. Let's see if we can eliminate
      // all others.
      elem.asInstanceOf[VT]
    else {
      try {
        throw new Exception("Trying to cast ElemValue " + elem + ", of type " + myType.displayName + " to " + expectedType.displayName)
      } catch {
        case e:Exception => play.api.Logger.error("", e); throw e
      }
    }
  }
}