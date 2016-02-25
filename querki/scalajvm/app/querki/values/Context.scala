package querki.values

import models._

import querki.ecology._

// TODO: this is a bad smell! Can we hide the parser better behind the Context?
import querki.ql.{QLCall, QLPhrase, QLParser, QLScopes}
import querki.util.DebugRenderable

case class QLContext(value:QValue, requestOpt:Option[RequestContext], parentOpt:Option[QLContext] = None, 
                     parser:Option[QLParser] = None, depth:Int = 0, useCollStack:Int = 0, propOpt:Option[Property[_,_]] = None,
                     currentValue:Option[DisplayPropVal] = None, 
                     fromTransformOpt:Option[Thing] = None, withCallOpt:Option[QLCall] = None,
                     scopes:Map[QLParser,QLScopes] = Map.empty)
                     (implicit val state:SpaceState, val ecology:Ecology)
  extends DebugRenderable with EcologyMember
{
  lazy val Core = interface[querki.core.Core]
  lazy val QL = interface[querki.ql.QL]
  
  def ExactlyOne = Core.ExactlyOne
  def Optional = Core.Optional
  def QList = Core.QList
  
  // This might become a config param -- it is the maximum depth we will allow a call to be. For now, we're
  // keeping it very tight, but it might eventually need to be over a thousand.
  val maxDepth = 100
  
  // Note that this will crash if we don't have a RequestContext!
  lazy val System = interface[querki.system.System]
  
  def request:RequestContext = {
    requestOpt match {
      case Some(r) => r
      case None => throw new Exception("Attempting to fetch the RequestContext from a Context that doesn't have one!")
    }
  }
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
   * Take a bunch of result contexts, concatenate them, and return them as the "next".
   */
  def concat(contexts:Iterable[QLContext]):QLContext = {
    contexts.headOption match {
      case Some(headContext) => {
        val resultType = headContext.value.pType
        val results = contexts.map(_.value.cv).flatten
        val coll = chooseCollection(results)
        next(coll.makePropValue(results, resultType))
      }
      // TODO: This isn't right: we really want the pType of what these would have contained. So this
      // function may not be at quite the right level?
      case _ => next(Core.emptyOpt(value.pType))
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
        val elemContext = next(ExactlyOne(elem)).clearAsCollection
        cb(elemContext)
      }
    }
  }
  
  /**
   * Maps the given function to each element in this context, and flattens the result.
   * 
   * IMPORTANT: if the useCollection flag is set for this context, then this calls the function only once,
   * with the collection context, *not* with each element! In other words, the definition of "map" depends
   * on the useCollection flag!
   */
  def mapOpt[T](cb:QLContext => Option[T]) = {
    if (useCollection) {
      val ret = cb(this)
      ret match {
        case Some(t) => List(t)
        case None => List.empty[T]
      }
    } else {
      value.cv flatMap { elem =>
        val elemContext = next(ExactlyOne(elem)).clearAsCollection
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
   * 
   * TODO: with the extremely rare exception of cases where we genuinely care about the useCollection
   * flag, this is now obviated by the various functions in Invocation. Can we get rid of it?
   */
  def flatMapAsContext[T <: ElemValue](cb:QLContext => Option[T], resultType:PType[_]):QLContext = {
    val ct = value.cType
    // TODO: this is an unfortunate cast. It's correct, but ick. Can we eliminate it?
    val raw = mapOpt(cb).asInstanceOf[ct.implType]
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
      if (!raw.isEmpty && (!ElemValue.matchesType(raw.head.pType, pt))) {
        raw.head.pType match {
          case errType:IsErrorType => ExactlyOne(raw.head)
          case _ => throw new Exception("Context.collect expected type " + pt + " but got " + raw.head.pType)
        }
      } else {
        val newCT = chooseCollection(raw)
        newCT.makePropValue(raw, pt)
      }
    }
  }
  
  def chooseCollection(raw:Iterable[ElemValue]):Collection = {
    // HACK: How should this work correctly? There seems to be a general concept trying to break out.
    // In general, collect (and many of these operations) are very monadically evil, but
    // we've consciously decided to live with that.
    value.cType match {
      case t:querki.core.CollectionCreation#ExactlyOne => {
        if (raw.isEmpty)
          Optional 
        else if (raw.size == 1)
          ExactlyOne
        else
          QList
      }
      case t:querki.core.CollectionCreation#Optional => if (raw.size > 1) QList else Optional
      case _ => value.cType
    }    
  }
  
  override def toString = "Context(" + value + ")@" + this.hashCode()
  
  def debugRender = /*"Context@" + this.hashCode() +*/ "(" + value.debugRender(this) + ")"
  
  /**
   * Convenience method to build the successor to this context, in typical chained situations.
   */
  def next(v:QValue) = copy(value = v, parentOpt = Some(this), depth = depth + 1)
  
  /**
   * Variant that indicates which "transformation" (Property or Function) caused this value to be calculated.
   */
  def nextFrom(v:QValue, transform:Thing) = 
    copy(value = v, parentOpt = Some(this), depth = depth + 1, fromTransformOpt = Some(transform))  
    
  /**
   * Variant that notes which Call we are currently processing.
   */
  def withCall(call:QLCall, transform:Thing) = copy(depth = depth + 1, withCallOpt = Some(call), fromTransformOpt = Some(transform))
  
  /**
   * Access to any page parameters from the metadata.
   */
  def requestParams = request.parsedParams
  
  /**
   * asCollection means "right now, evaluate the very next operation as a collection."
   */
  def asCollection = copy(parentOpt = Some(this), depth = depth + 1, useCollStack = 1)
  /**
   * forceAsCollection means "evaluate the operation that is contained somewhere down inside this as a collection."
   */
  def forceAsCollection = copy(parentOpt = Some(this), depth = depth + 1, useCollStack = 2)
  /**
   * This seems a little delicate, and it is. clearAsCollection is dealing with the tension where the
   * inner Text of this phrase:
   * {{{
   *     * ""Some header info: [[""An interior item: ____""]]""
   * }}}    
   * gets handled element-by-element, but the parameter in this:
   * {{{
   *     _section(""My header"", ""An interior item: ____"")
   * }}}    
   * is handled at the list level.
   * 
   * Basically, forceAsCollection tells the system that the next clearAsCollection won't do it -- you have
   * to go down a second level before it actually will clear the flag.
   */
  def clearAsCollection = copy(parentOpt = Some(this), depth = depth + 1, 
      useCollStack = { if (useCollStack > 0) useCollStack - 1 else 0 })
  def useCollection = useCollStack > 0
  
  def forProperty(prop:Property[_,_]) = copy(parentOpt = Some(this), depth = depth + 1, propOpt = Some(prop))
  
  def withState(newState:SpaceState) = copy()(newState, ecology)
  
  def withScopes(p:QLParser, newScopes:QLScopes) = copy(scopes = scopes + (p -> newScopes))
  
  /**
   * The Text Property that we are currently processing. Mainly used for error reporting, currently.
   */
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
   * Looks up the stack to find the Property of the specified Type. Usually used to figure out where a
   * value came from.
   */
  def fromPropertyOfType[VT](pt:PType[VT]):Option[Property[VT,_]] = {
    def tryParent:Option[Property[VT,_]] = parentOpt.flatMap(_.fromPropertyOfType(pt))
    
    fromTransformOpt match {
      case Some(prop:Property[_,_]) => prop.confirmType(pt).orElse(tryParent)
      case _ => tryParent
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
  def apply(request:RequestContext)(implicit state:SpaceState, ecology:Ecology) = QLContext(EmptyValue.untyped, Some(request))
}

/**
 * The context to use when you know absolutely nothing, not even a RequestContext. In general, this is
 * a design bug, and should go away eventually. (We are keeping it around because it is used under the
 * hood of Thing.displayName, indirectly, which will take some work to fix.)
 * 
 * TODO: deprecate and remove this.
 */
object EmptyContext {
  def apply(implicit ecology:Ecology) = QLContext(EmptyValue.untyped, None)(ecology.api[querki.system.System].State, ecology)
}