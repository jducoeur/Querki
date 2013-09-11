package querki.values

import controllers.RequestContext

import models._
import models.system._

import ql.QLParser

import querki.values._

import play.api.Logger

trait ContextBase {
  def value:QValue
  def state:SpaceState = request.state.getOrElse(SystemSpace.State)
  def requestOpt:Option[RequestContext]
  def request:RequestContext = {
    requestOpt match {
      case Some(r) => r
      case None => throw new Exception("Attempting to fetch the RequestContext from a Context that doesn't have one!")
    }
  }
  def parentOpt:Option[ContextBase]
  // Parent matters at rendering time -- we render the final context in the context of its parent.
  // This matter most when (as often), the last context is a Text; it needs to be rendered correctly
  // in its parent context.
  // Note that, if this Context is the root, we just use it as the "parent". Be careful of possible loops!
  // You can not simply walk up this tree!
  def parent:ContextBase = {
    parentOpt match {
      case Some(p) => p
      case None => this
    }
  }
  def parser:Option[QLParser]
  def useCollection:Boolean = false
  
  // The Property that we are currently rendering. Not always set, but we try to do so:
  def propOpt:Option[Property[_,_]]
  
  def depth:Int
  // This might become a config param -- it is the maximum depth we will allow a call to be. For now, we're
  // keeping it very tight, but it might eventually need to be over a thousand.
  val maxDepth = 100
  
  def isEmpty = value.isEmpty
  
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
      value.cv map { elem =>
        val elemContext = next(ExactlyOne(elem))
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
      value.cv flatMap { elem =>
        val elemContext = next(ExactlyOne(elem))
        cb(elemContext)
      }
    }
  }
  
  /**
   * Similar to ordinary map(), but produces a new Context with the result.
   */
  def flatMapAsContext[T <: ElemValue](cb:ContextBase => Option[T], resultType:PType[_]):ContextBase = {
    val ct = value.cType
    // TODO: this is an unfortunate cast. It's correct, but ick. Can we eliminate it?
    val raw = flatMap(cb).asInstanceOf[ct.implType]
    val propVal = ct.makePropValue(raw, resultType)
    next(propVal)
  }
  
  def flatMapAsValue[T <: ElemValue](cb:ContextBase => QValue):QValue = {
    if (isEmpty) {
      EmptyValue.untyped
    } else {
      val ct = value.cType
      val qvs = map(cb)
      // TODO: this is an unfortunate cast. It's correct, but ick. Can we eliminate it?
      val raw = qvs.flatten(_.cv).asInstanceOf[ct.implType]
      ct.makePropValue(raw, qvs.head.pType)
    }
  }
  
  override def toString = "Context(" + value + ")@" + this.hashCode()
  
  def debugRender = "Context@" + this.hashCode() + "(" + value.debugRender(this) + ")"
  
  /**
   * Convenience method to build the successor to this context, in typical chained situations.
   */
  def next(v:QValue) = QLContext(v, requestOpt, Some(this), parser, depth + 1)
  
  def asCollection = QLContext(value, requestOpt, Some(this), parser, depth + 1, true)
  
  def forProperty(prop:Property[_,_]) = QLContext(value, requestOpt, Some(this), parser, depth + 1, useCollection, Some(prop))
  
  def getProp:Option[Property[_,_]] = {
    propOpt match {
      case Some(prop) => Some(prop)
      case None => {
        parentOpt match {
          case Some(p) => parent.getProp
          case None => None
        }
      }
    }
  }
  
  /**
   * Returns the root of the context tree. Mainly so that parameters can start again with the same root.
   */
  def root:ContextBase = {
    parentOpt match {
      case Some(p) => p.root
      case None => this
    }
  }
      
  def isCut = value.cut
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext(value:QValue, requestOpt:Option[RequestContext], parentOpt:Option[ContextBase] = None, 
                     parser:Option[QLParser] = None, depth:Int = 0, listIn:Boolean = false, propOpt:Option[Property[_,_]] = None) extends ContextBase {
  override def useCollection = listIn
}

/**
 * Convenience wrapper, for cases where all you have is the request.
 */
object QLRequestContext {
  def apply(request:RequestContext) = QLContext(EmptyValue.untyped, Some(request))
}

object EmptyContext extends QLContext(EmptyValue.untyped, None)