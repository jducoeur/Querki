package ql

import controllers.RequestContext

import models._
import models.system._

abstract class ContextBase {
  def value:TypedValue
  def state:SpaceState
  def request:RequestContext
  // Parent matters at rendering time -- we render the final context in the context of its parent.
  // This matter most when (as often), the last context is a Text; it needs to be rendered correctly
  // in its parent context.
  def parent:ContextBase
  def parser:Option[QLParser]
  def useCollection:Boolean = false
  
  // The Property that we are currently rendering. Not always set, but we try to do so:
  def propOpt:Option[Property[_,_]]
  
  def depth:Int
  // This might become a config param -- it is the maximum depth we will allow a call to be. For now, we're
  // keeping it very tight, but it might eventually need to be over a thousand.
  val maxDepth = 100
  
  def isEmpty = value.v.isEmpty
  
  /**
   * Maps the given function to each element in this context.
   * 
   * IMPORTANT: if the useCollection flag is set for this context, then this calls the function only once,
   * with the collection context, *not* with each element! In other words, the definition of "map" depends
   * on the useCollection flag!
   */
  def map[T](cb:ContextBase => T) = {
    if (useCollection) {
      List(cb(this))
    } else {
      value.v.cv map { elem =>
        val elemContext = next(TypedValue(ExactlyOne(elem), value.pt))
        cb(elemContext)
      }
    }
  }
  
  /**
   * Maps the given function to each element in this context, and flattens the result.
   * 
   * TODO: this isn't quite right, monadically speaking -- it's shouldn't assume Option. At some
   * point when I can think it through better, make the signatures here more correct. But Option is
   * the case I usually care about.
   * 
   * IMPORTANT: if the useCollection flag is set for this context, then this calls the function only once,
   * with the collection context, *not* with each element! In other words, the definition of "map" depends
   * on the useCollection flag!
   */
  def flatMap[T](cb:ContextBase => Option[T]) = {
    if (useCollection) {
      val ret = cb(this)
      ret match {
        case Some(t) => List(t)
        case None => List.empty[T]
      }
    } else {
      value.v.cv flatMap { elem =>
        val elemContext = next(TypedValue(ExactlyOne(elem), value.pt))
        cb(elemContext)
      }
    }
  }
  
  /**
   * Similar to ordinary map(), but produces a new Context with the result.
   */
  def flatMapAsContext[T <: ElemValue](cb:ContextBase => Option[T], resultType:PType[_]):ContextBase = {
    val ct = value.ct
    // TODO: this is an unfortunate cast. It's correct, but ick. Can we eliminate it?
    val raw = flatMap(cb).asInstanceOf[ct.implType]
    val propVal = ct.makePropValue(raw)
    next(TypedValue(propVal, resultType))
  }
  
  override def toString = "Context(" + value.v + ")@" + this.hashCode()
  
  /**
   * Convenience method to build the successor to this context, in typical chained situations.
   */
  def next(v:TypedValue) = QLContext(v, request, Some(this), parser, depth + 1)
  
  def asCollection = QLContext(value, request, Some(this), parser, depth + 1, true)
  
  def forProperty(prop:Property[_,_]) = QLContext(value, request, Some(this), parser, depth + 1, useCollection, Some(prop))
  
  def getProp:Option[Property[_,_]] = {
    propOpt match {
      case Some(prop) => Some(prop)
      case None => {
        if (parent == this) {
          None
        } else {
          parent.getProp
        }
      }
    }
  }
  
  /**
   * Returns the root of the context tree. Mainly so that parameters can start again with the same root.
   */
  def root:ContextBase = 
    if (parent == this)
      this
    else
      parent.root
      
  def isCut = value.cut
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext(value:TypedValue, request:RequestContext, parentIn:Option[ContextBase] = None, 
                     parser:Option[QLParser] = None, depth:Int = 0, listIn:Boolean = false, propOpt:Option[Property[_,_]] = None) extends ContextBase {
  def state = request.state.getOrElse(SystemSpace.State)
  def parent = parentIn match {
    case Some(p) => p
    case None => this
  }
  override def useCollection = listIn
}

case class QLRequestContext(request:RequestContext) extends ContextBase {
  def state = request.state.getOrElse(SystemSpace.State)
  def value:TypedValue = throw new Exception("Can't use the contents of QLRequestContext!")  
  def parent:ContextBase = throw new Exception("QLRequestContext doesn't have a parent!")
  def parser:Option[QLParser] = throw new Exception("QLRequestContext doesn't have a parser!")
  def depth:Int = 0
  def propOpt = None
}

/**
 * This should only be used in cases where we really don't have a context -- generally,
 * displaying outside of a proper page display in a Space. It is unlikely to work for
 * any sophisticated properties, but can be used for PlainText and the like.
 */
case object EmptyContext extends ContextBase {
  def value:TypedValue = throw new Exception("Can't use the contents of EmptyContext!")
  def state:SpaceState = throw new Exception("Can't use the space of EmptyContext!")
  def request:RequestContext = throw new Exception("Can't get the request of EmptyContext!")
  def parent:ContextBase = throw new Exception("EmptyContext doesn't have a parent!")
  def parser:Option[QLParser] = throw new Exception("Can't get a parser from EmptyContext!")
  def depth:Int = 0
  def propOpt = None
}
