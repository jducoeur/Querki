package querki.values

import models._
import models.system._

import ql.QLParser

import querki.ecology._
import querki.values._

import play.api.Logger

// TODO: this trait probably belongs with QLog
trait DebugRenderable {
  def debugRender:String
}

case class QLContext(value:QValue, requestOpt:Option[RequestContext], parentOpt:Option[QLContext] = None, 
                     parser:Option[QLParser] = None, depth:Int = 0, useCollection:Boolean = false, propOpt:Option[Property[_,_]] = None) 
  extends DebugRenderable with EcologyMember
{
  // This might become a config param -- it is the maximum depth we will allow a call to be. For now, we're
  // keeping it very tight, but it might eventually need to be over a thousand.
  val maxDepth = 100
  
  // Note that this will crash if we don't have a RequestContext!
  lazy val System = interface[querki.system.System]
  
  def ecology:Ecology = request.ecology
  def request:RequestContext = {
    requestOpt match {
      case Some(r) => r
      case None => throw new Exception("Attempting to fetch the RequestContext from a Context that doesn't have one!")
    }
  }
  def state:SpaceState = request.state.getOrElse(System.State)
  def isEmpty = value.isEmpty
  // Parent matters at rendering time -- we render the final context in the context of its parent.
  // This matter most when (as often), the last context is a Text; it needs to be rendered correctly
  // in its parent context.
  // Note that, if this Context is the root, we just use it as the "parent". Be careful of possible loops!
  // You can not simply walk up this tree!
  def parent:QLContext = {
    parentOpt match {
      case Some(p) => p
      case None => this
    }
  }
  
  /**
   * Maps the given function to each element in this context.
   * 
   * IMPORTANT: if the useCollection flag is set for this context, then this calls the function only once,
   * with the collection context, *not* with each element! In other words, the definition of "map" depends
   * on the useCollection flag!
   */
  def map[T](cb:QLContext => T) = {
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
  def flatMap[T](cb:QLContext => Option[T]) = {
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
   * 
   * TODO: this is fundamentally suspicious. What if ct is ExactlyOne, and cb returns None? We'll
   * wind up with an empty ExactlyOne, which violates the invariants. See collect, below, for a
   * more sensible approach.
   */
  def flatMapAsContext[T <: ElemValue](cb:QLContext => Option[T], resultType:PType[_]):QLContext = {
    val ct = value.cType
    // TODO: this is an unfortunate cast. It's correct, but ick. Can we eliminate it?
    val raw = flatMap(cb).asInstanceOf[ct.implType]
    val propVal = ct.makePropValue(raw, resultType)
    next(propVal)
  }

  /**
   * Given a function that takes a single-element context and produces a QValue, this runs that
   * over all of the elements in this context, and collects the results into a single QValue.
   * 
   * This needs the PType of the expected result, so that it can set the results up correctly
   * if they are empty.
   * 
   * Note that we try to return the same Collection as the incoming, but deliberately don't try
   * if the results of the function return the wrong ordinal amount.
   */
  def collect(pt:PType[_])(cb:QLContext => QValue):QValue = {
    if (isEmpty) {
      EmptyValue(pt)
    } else {
      val qvs = map(cb)
      val raw = qvs.flatten(_.cv)
      if (!raw.isEmpty && (!raw.head.matchesType(pt)))
        throw new Exception("Context.collect expected type " + pt + " but got " + raw.head.pType)
      val newCT = 
        // HACK: How should this work correctly? There seems to be a general concept trying to break out.
        // In general, collect (and many of these operations) are very monadically evil, but
        // we've consciously decided to live with that.
        value.cType match {
          case ExactlyOne => {
            if (raw.isEmpty)
              Optional 
            else if (raw.size == 1)
              ExactlyOne
            else
              QList
          }
          case Optional => if (raw.size > 1) QList else Optional
          case _ => value.cType
        }
      newCT.makePropValue(raw, pt)
    }
  }
  
  override def toString = "Context(" + value + ")@" + this.hashCode()
  
  def debugRender = "Context@" + this.hashCode() + "(" + value.debugRender(this) + ")"
  
  /**
   * Convenience method to build the successor to this context, in typical chained situations.
   */
  def next(v:QValue) = copy(value = v, parentOpt = Some(this), depth = depth + 1)
  
  def asCollection = copy(parentOpt = Some(this), depth = depth + 1, useCollection = true)
  
  def forProperty(prop:Property[_,_]) = copy(parentOpt = Some(this), depth = depth + 1, propOpt = Some(prop))
  
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
  def root:QLContext = {
    parentOpt match {
      case Some(p) => p.root
      case None => this
    }
  }
      
  def isCut = value.cut
}

/**
 * Convenience wrapper, for cases where all you have is the request. This is mainly intended for use from the
 * Play template level.
 */
object QLRequestContext {
  def apply(request:RequestContext) = QLContext(EmptyValue.untyped, Some(request))
}

/**
 * The context to use when you know absolutely nothing, not even a RequestContext. In general, this is
 * a design bug, and should go away eventually. (We are keeping it around because it is used under the
 * hood of Thing.displayName, indirectly, which will take some work to fix.)
 * 
 * TODO: deprecate and remove this.
 */
object EmptyContext extends QLContext(EmptyValue.untyped, None)