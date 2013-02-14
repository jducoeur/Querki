package models

import scala.xml._

import play.api.templates.Html

import Thing._

import ql._

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
  protected def doDeserialize(ser:String):VT
  final def deserialize(ser:String):ElemValue = ElemValue(doDeserialize(ser))
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  protected def doSerialize(v:VT):String
  final def serialize(v:ElemValue):String = doSerialize(get(v))
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  protected def doRender(context:ContextBase)(v:VT):Wikitext
  final def render(context:ContextBase)(v:ElemValue):Wikitext = doRender(context)(get(v))
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  protected def doDefault:VT
  final def default:ElemValue = ElemValue(doDefault)
  
  /**
   * Parses text input from the user. By default, we assume that this is the same
   * as deserialization, but override this when that's not true.
   * 
   * This should throw an Exception if the input is not legal. This is used in
   * validation.
   */
  protected def doFromUser(str:String):VT = doDeserialize(str)
  final def fromUser(str:String):ElemValue = ElemValue(doFromUser(str))
  
  /**
   * Turns this value into an appropriate form for user editing. Currently that means
   * a String, although that's likely to get more interesting soon.
   */
  protected def doToUser(v:VT):String = doSerialize(v)
  final def toUser(v:ElemValue):String = doToUser(get(v))
  
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
  def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Html = {
    val xmlRaw = renderInputXml(prop, state, currentValue, v)
    val xml2 = xmlRaw %
    	Attribute("name", Text(currentValue.inputControlId),
    	Attribute("id", Text(currentValue.inputControlId), Null))
    Html(xml2.toString)
  }
}

trait PTypeBuilder[VT, -RT] {
  
  type rawType = RT
  
  def wrap(raw:RT):VT
  def apply(raw:RT):ElemValue = ElemValue(wrap(raw))  
}
trait SimplePTypeBuilder[VT] extends PTypeBuilder[VT, VT] {
  def wrap(raw:VT) = raw
}
trait NullTypeBuilder[VT] extends PTypeBuilder[VT, Nothing] {
  def wrap(raw:Nothing) = throw new Exception("Can't call NullTypeBuilder!")
}
