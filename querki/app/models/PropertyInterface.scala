package models

/**
 * This defines essentially the "declaration" of a Property, separate from its implementation.
 * It exists for one specific reason: to break compile-time dependency cycles. Modules should expose
 * PropertyInterface to the rest of the world, *not* the Properties themselves, so that other
 * Modules (and the core) can use them without creating cycles.
 * 
 * Note that none of this has anything to do with avoiding *runtime* (initialization) dependency
 * cycles. That has an entirely different mechanism, around initialization order.
 */
class PropertyInterface[VT, RT] {
  private var impl:Option[Property[VT,RT]] = None
  
  def set(prop:Property[VT,RT]) = {
    impl match {
      case None => impl = Some(prop)
      case Some(previous) => throw new Exception(s"Trying to re-set PropertyInterface to $prop; was $impl")
    }
  }
  
  def get = {
    impl match {
      case Some(prop) => prop
      case _ => throw new Exception("Trying to access PropertyInterface before it is initialized!")
    }
  }
}

object PropertyInterface {
  implicit def interface2Property[VT,RT](interface:PropertyInterface[VT,RT]):Property[VT,RT] = {
    interface.get
  }  
}
