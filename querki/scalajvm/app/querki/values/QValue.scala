package querki.values

import scala.concurrent.Future

import models._

// TODO: both of these should get evolved away!
import play.api.Logger

import querki.ecology.Ecology
import querki.util._
import querki.values._

/**
 * Marker trait, to indicate that we should stop processing at this value. Mix it
 * into the returned value to indicate that we should stop. This is probably a stopgap,
 * but it's okay for now.
 */
trait CutProcessing

/**
 * Marker trait for PTypes that represent an error. Used to avoid re-erroring when we encounter this.
 */
trait IsErrorType

trait QValue {
  // We are cutting iff the constructor mixed in CutProcessing:
  def cut = this.isInstanceOf[CutProcessing]
  
  // Expose special behaviour so that we can express "this Property has been deleted":
  def isDeleted:Boolean = false
  
  val cType:Collection
  type cType = cType.implType
  def cv:cType
  
  def pType:PType[_]
  
  // TODO: this doesn't need to take elemT any more:
  def serialize(elemT:PType[_])(implicit state:SpaceState):String = cType.doSerialize(cv, elemT)
  // DEPRECATED: in favor of firstOpt
  def first = cType.first(this)
  def firstOpt = cType.firstOpt(this)
  // DEPRECATED: in favor of firstAs()
  def firstTyped[VT](elemT:PType[VT]):Option[VT] = if (isEmpty) None else Some(elemT.get(first))
  def firstAs[VT](elemT:PType[VT]):Option[VT] = {
    if (isEmpty)
      None
    else
      first.getOpt(elemT)
  }
  def wikify(context:QLContext, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = 
    pType.fullWikify(context, this, displayOpt, lexicalThing).getOrElse(
      cType.doWikify(context)(cv, pType, displayOpt, lexicalThing))
  
  /**
   * Fetch the nth value in here, which is expected to be the specified type.
   */
  def nthAs[VT](index:Int, elemT:PType[VT]):Option[VT] = {
    val nth = cType.get(this, index)
    nth.flatMap(_.getOpt(elemT))
  }
  
  /**
   * Returns a new QValue with the nth element replaced.
   */
  def replaceNth(index:Int, elem:ElemValue):QValue = {
    cType.makePropValue(cType.replace(cv, elem, index), pType)
  }
  
  def isEmpty = cType.isEmpty(this)
  def size = cv.size

  /**
   * Returns true iff this value is of the given Type *or* can be automatically coerced to it by the various
   * operations.
   */
  def matchesType(pt:PType[_]):Boolean = ElemValue.matchesType(pType, pt)
  
  /**
   * Returns true iff these two QValues are equivalent.
   * 
   * TODO: this is probably subject to the same subclassing problems as == in Scala. Think it through
   * more carefully, and see if this logic makes sense in terms of subclassing.
   * 
   * TODO: this was originally very strict about matching cType and pType. I've loosened it all the way
   * to the other end -- we are completely loose about cType, and as loose as possible about pType. Let's
   * see if that introduces any problems.
   */
  def matches(other:QValue):Boolean = {
    if (isDeleted != other.isDeleted)
      false
    else if (!matchesType(other.pType))
      false
    else if (cv.size != other.cv.size)
      false
    else {
      val pairs = cv.zip(other.cv)
      pairs.forall { pair =>
        pType.matches(pair._1, pair._2)
      }
    }
  }
  
  def debugRender(context:QLContext) = {
    cType.getClass().getSimpleName() + "[" + pType.getClass().getSimpleName() + "]" + "(" + cType.debugRender(context)(cv, pType) + ")"
  }
  
  // Returns the raw Iterable of ElemValues. Not often the right things to do, unless you
  // specifically don't care about type.
  def elems = cv
  
  /**********************
   * CASTING METHODS
   * 
   * These methods take a PType parameter mainly so that they can use the underlying VT. In
   * principle, we shouldn't be doing anything with that elemT parameter, but we *should*
   * be checking that it matches the actual PType of this QValue. (Using the same definition
   * of "matching" as in ElemValue.)
   * 
   * TODO: do that pType matches elemT check in all of these!
   **********************/
  // TODO: this isn't really flatMap, and shouldn't be named flatMap. Can we make things more
  // properly monadic?
  def flatMap[VT, T](elemT:PType[VT])(cb:VT => Option[T]) = cv.flatMap { elem => 
    val vt = elemT.get(elem)
    cb(vt)
  }
  
  /**
   * The primary transformer from one QValue to another. Takes a function that transforms the underlying
   * data types, and does the unwrapping, transforming of each element, and re-wrapping.
   * 
   * Note that, if you're just trying to convert one type to another in the default way with no transform,
   * just use coerceTo instead.
   */
  def map[VT, DT, RT](sourceType:PType[VT], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT):QValue = {
    val iter = cv.map { elem => 
      val vt = sourceType.get(elem)
      val result = cb(vt)
      destType(result)
    }
    cType.makePropValue(iter, destType)
  }
  
  /**
   * Attempts to coerce this Value to the other PType, if it knows how to do so.
   */
  def coerceTo(destType:PType[_]):Option[QValue] = {
    if (pType.canCoerceTo(destType)) {
      val iter = cv.map (elem => pType.coerceTo(destType, elem))
      Some(cType.makePropValue(iter, destType))
    } else
      None
  }
  
  def rawList[VT](elemT:PType[VT]):List[VT] = {
    (List.empty[VT] /: cv) ((list, elem) => list :+ elemT.get(elem))
  }
  
  def contains[VT](elemT:PType[VT], toCheck:VT):Boolean = cv.exists { elem =>
    val vt = elemT.get(elem)
    elemT.doMatches(vt, toCheck)
  }
  
  def contains(toCheck:ElemValue):Boolean = cv.exists { elem => 
    pType.matches(elem, toCheck) 
  }
  
  def exists[VT](elemT:PType[VT], check:VT => Boolean):Boolean = cv.exists { elem =>
    val vt = elemT.get(elem)
    check(vt)
  }
  
  def filter[VT](elemT:PType[VT], f:VT => Boolean):QValue = {
    cType.makePropValue(cv.filter { elem => f(elem.get(elemT)) }, elemT)
  }
  
  def indexOf(toCheck:ElemValue):Option[Int] = {
    val pt = toCheck.pType
    if (pt != pType) {
      None
    } else {
      val index = cv.toList.indexWhere { elem =>
        pt.matches(elem, toCheck)
      }
      index match {
        case -1 => None
        case n:Int => Some(n)
      }
    }
  }
  
  def elemAt(index:Int):ElemValue = cv.toList(index)
  
  /**
   * See Collection.append() for details on what this does.
   */
  def append(elem:ElemValue):(QValue,Option[ElemValue]) = {
    if (!ElemValue.matchesType(elem.pType, pType))
      throw new Exception("QValue.append got the wrong type!")
    
    cType.append(cv, elem)
  }
  
  /**
   * The approximate size in memory of this value. See comments on Thing.qsize for more details.
   */
  lazy val memsize:Int = {
    // Note that we are intentionally not worrying about the overhead of the Collection, in the
    // interest of low-balling the estimate a bit.
    elems.map { pType.computeMemSize(_) }.sum
  }
  
  override def toString = {
    cType.displayName + " " + pType.displayName + ": " + cv
  }
}

object QValue {
  def make[DT, RT](cType:Collection, pType:PType[DT] with PTypeBuilder[DT, RT], vals:RT*):QValue = {
    val iter = vals.map(pType(_))
    cType.makePropValue(iter, pType)
  }
}
object EmptyValue {
  // TODO: do something with this?
  def apply(pType:PType[_])(implicit ecology:Ecology) = ecology.api[querki.core.Core].emptyListOf(pType)
  // Is this best considered to be Unit, or Bottom? I think it's roughly Bottom -- it can be considered
  // to be any type -- but it's a bit weird in type-system terms.
  def untyped(implicit ecology:Ecology) = ecology.api[querki.core.Core].emptyList
}
