package models

import scala.xml._

import play.api.templates.Html

import Thing._

import ql._

import querki.values._

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType[VT](i:OID, s:OID, m:OID, pf:PropFetcher) extends Thing(i, s, m, Kind.Type, pf) {
  
  type valType = VT

  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  def doDeserialize(ser:String):VT
  final def deserialize(ser:String):ElemValue = ElemValue(doDeserialize(ser))
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  def doSerialize(v:VT):String
  final def serialize(v:ElemValue):String = doSerialize(get(v))
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def doRender(context:ContextBase)(v:VT):Wikitext
  final def render(context:ContextBase)(v:ElemValue):Wikitext = doRender(context)(get(v))
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  def doDefault:VT
  final def default:ElemValue = ElemValue(doDefault)
  
  /**
   * Turns this value into an appropriate form for user editing. Currently that means
   * a String, although that's likely to get more interesting soon.
   */
  def doToUser(v:VT):String = doSerialize(v)
  final def toUser(v:ElemValue):String = doToUser(get(v))
  
  /**
   * This compares two values. It is used to sort Collections.
   */
  def doComp(context:ContextBase)(left:VT, right:VT):Boolean = { math.Ordering.String.lt(doToUser(left), doToUser(right)) } 
  final def comp(context:ContextBase)(left:ElemValue, right:ElemValue):Boolean = doComp(context)(get(left), get(right))
  
  /**
   * The type unwrapper -- takes an opaque ElemValue and returns the underlying value.
   * This is a fundamentally unsafe operation, so it should always be performed in the
   * context of a Property.
   */
  def get(v:ElemValue):VT = v.elem.asInstanceOf[VT]
  
  /**
   * Can this String value be legitimately interpreted as this type?
   * 
   * This is closely related to doFromUser -- iff something can be parsed by doFromUser, it
   * should validate cleanly. It is intended for UI use.
   */
  final def validate(v:String):Boolean = try {
    val dummy = doFromUser(v)
    true
  } catch {
    case _:Exception => false
  }
  
  /**
   * Display the appropriate input control for this type, with the default value set as specified.
   * 
   * All Types that can be user-input should define this.
   * 
   * TBD: in practice, I don't love this. The coupling is roughly correct, but it winds up mixing
   * HTML-specific code into a very primitive level of the system. There should probably instead be
   * side classes for each PType, which describe how to render them in particular circumstances. But
   * we'll get to that...
   */
  def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):scala.xml.Elem
  def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
    renderInputXml(prop, state, currentValue, v)
  }
  
  /**
   * Parses form input from the user. By default, we assume that this is the same
   * as deserialization, but override this when that's not true.
   * 
   * This should throw an Exception if the input is not legal. This is used in
   * validation.
   */
  protected def doFromUser(str:String):VT = doDeserialize(str)
  final def fromUser(str:String):ElemValue = ElemValue(doFromUser(str))
  
  /**
   * If this Type implies special processing when named in a QL expression (other than simply returning
   * the value of the property), override this method
   * The guts of applying a QL function. Note that this allows two contexts, which is often relevant
   * for these:
   * 
   *   incomingContext -> definingContext.prop(params)
   *   
   * If this isn't partially applied, the incomingContext is used for both. See Property for the main
   * usage of this.
   */
  def qlApplyFromProp(definingContext:ContextBase, incomingContext:ContextBase, prop:Property[VT,_], params:Option[Seq[QLPhrase]]):Option[TypedValue] = None
  
  /**
   * Iff defined, this Type must *always* be used with the specified Collection.
   * 
   * This is mostly intended for use with Type Aliases.
   */
  def requiredColl:Option[Collection] = None
  
  /**
   * Types can override this to provide default renderings when you look at a Property of this Type.
   */
  def renderProperty(prop:Property[_,_])(implicit request:controllers.RequestContext):Option[Wikitext] = None
  
  /**
   * The PType-math version of ==; this is here so that specific PTypes can override it.
   */
  def matches(left:ElemValue, right:ElemValue):Boolean = {
    matches(get(left), get(right))
  }
  def matches(left:VT, right:VT):Boolean = {
    left == right
  }
}

trait PTypeBuilderBase[VT, -RT] {
  
  def pType:PType[VT]
  
  type rawType = RT
  
  def wrap(raw:RT):VT
  def apply(raw:RT):ElemValue = ElemValue(wrap(raw))  
}
trait PTypeBuilder[VT, -RT] extends PTypeBuilderBase[VT, RT] { this:PType[VT] =>
  def pType = this
}
trait SimplePTypeBuilder[VT] extends PTypeBuilder[VT, VT] { this:PType[VT] =>
  def wrap(raw:VT) = raw
}
