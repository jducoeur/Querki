package querki.values

/**
 * The value of a primitive Type. These are always considered "elements", since they
 * are always wrapped inside Collections.
 * 
 * Note that ElemValue is untyped. The is necessary -- if we try to keep this strongly
 * typed, then the matrix composition of Collections and PTypes becomes impossible at
 * runtime. So ElemValues are fundamentally not typesafe, and should only be evaluated
 * in the context of their associated PTypes.
 * 
 * TODO: at some point, re-evaluate this. I have a suspicion that clever use of
 * Type Constraints might be able to work around the problems, but I'm not sure.
 */
case class ElemValue(elem:Any)
