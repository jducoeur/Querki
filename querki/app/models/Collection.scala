package models

import language.implicitConversions
import scala.xml._

import play.api.Logger

import Thing._

import ql._

import querki.ecology.Ecology
import querki.util._
import querki.values._

/**
 * A Collection is the Querki equivalent of a Functor in Category Theory. Properties
 * always combine a Type *and* a Collection. (You must explicitly state both Optional
 * or ExactlyOne.) 
 * 
 * As of this writing, it isn't obvious that Collections will be
 * required to be strictly monadic, but they probably all are. By being rigorous
 * and consistent about this, we make it much easier to write QL safely -- each
 * QL step is basically a flatMap.
 * 
 * TODO: for the time being, we're not bothering with modTime on Collections, since they
 * can't be user-created yet. When we add user-defined Collections, we'll need to make it
 * possible to fetch the timestamp at load time, as usual.
 */
abstract class Collection(i:OID, s:OID, m:OID, pf:PropFetcher)(implicit e:Ecology) extends Thing(i, s, m, Kind.Collection, pf, querki.time.epoch)(e) {
  
  type pType = PType[_]
  type implType <: Iterable[ElemValue]
  
  /**
   * Each Collection is required to implement this -- it is the deserializer for the
   * type.
   */
  protected def doDeserialize(ser:String, elemT:pType):implType
  final def deserialize(ser:String, elemT:pType):QValue = makePropValue(doDeserialize(ser,elemT), elemT)
  
  /**
   * Also required for all Collections, to serialize values of this type.
   */
  def doSerialize(v:implType, elemT:pType):String 
//  final def serialize(v:PropValue, elemT:pType):String = doSerialize(v.cv, elemT)
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None):Wikitext
  
  /**
   * Takes a value of this type, and renders it for showing in debug messages.
   */
  def debugRender(context:QLContext)(v:implType, elemT:pType):String = v.map(elemT.debugRender(context)(_)).mkString(", ")
  
  /**
   * Also required for all Collections -- the default value to fall back on.
   */
  protected def doDefault(elemT:pType):implType
  final def default(elemT:pType):QValue = makePropValue(doDefault(elemT), elemT)
  
  /**
   * Convenience wrapper for creating in-code PropValues.
   */
  def wrap(elem:ElemValue):implType
  def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue
  def apply(elem:ElemValue):QValue = makePropValue(wrap(elem), elem.pType)
  
  /**
   * Collections must implement this -- it builds the HTML representation of how to input this
   * collection.
   * 
   * TODO: this is an abstraction break, and really belongs in some side tree that maps Collections
   * to HTML representations. But that's for another day.
   */
  def doRenderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):Elem
  def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):Elem = {
    doRenderInput(prop, state, currentValue, elemT)
  }

  import play.api.data.Form
  def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, state:SpaceState):FormFieldInfo
  
  /**
   * TODO: this needs to become much more sophisticated, but it's a start.
   */
  def fromUser(newVal:String, prop:Property[_,_], elemT:pType):QValue = {
    apply(elemT.fromUser(newVal))
  }
  
  /**
   * Returns the head of the collection.
   * 
   * DEPRECATED: this will throw an exception if you call it on an empty collection! It is the
   * equivalent of Option.get. Use firstOpt instead.
   */
  final def first(v:QValue):ElemValue = v.cv.head
  final def firstOpt(v:QValue):Option[ElemValue] = if (isEmpty(v)) None else Some(v.cv.head)
  
  final def isEmpty(v:QValue):Boolean = v.cv.isEmpty
  
  implicit def toIterable(v:implType):Iterable[ElemValue] = v.asInstanceOf[Iterable[ElemValue]]
}
