package models

import play.api.templates.Html

import Thing._

import system._
import system.OIDs._
import system.SystemSpace._

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

trait PropValue {
  type myCollection <: Collection
  type cType = coll.implType
  type pType = PType[_]
  
  val coll:myCollection
  def cv:cType
  
  def serialize(elemT:pType):String = coll.doSerialize(cv, elemT)
}

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
  def renderInput(prop:Property[_,_], state:SpaceState, currentValue:Option[String]):Html = throw new Exception("I don't yet know how to display input for " + this)
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

/**
 * A Collection is the Querki equivalent of a Functor in Category Theory. Properties
 * always combine a Type *and* a Collection. (You must explicitly state both Optional
 * or ExactlyOne.) 
 * 
 * As of this writing, it isn't obvious that Collections will be
 * required to be strictly monadic, but they probably all are. By being rigorous
 * and consistent about this, we make it much easier to write QL safely -- each
 * QL step is basically a flatMap.
 */
abstract class Collection(i:OID, s:OID, m:OID, pf:PropFetcher) extends Thing(i, s, m, Kind.Collection, pf) {
  
  type pType = PType[_]
  type implType <: Iterable[ElemValue]
  
  /**
   * Each Collection is required to implement this -- it is the deserializer for the
   * type.
   */
  protected def doDeserialize(ser:String, elemT:pType):implType
  final def deserialize(ser:String, elemT:pType):PropValue = makePropValue(doDeserialize(ser,elemT))
  
  /**
   * Also required for all Collections, to serialize values of this type.
   */
  def doSerialize(v:implType, elemT:pType):String 
//  final def serialize(v:PropValue, elemT:pType):String = doSerialize(v.cv, elemT)
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def render(context:ContextBase)(v:PropValue, elemT:pType):Wikitext = {
    val renderedElems = v.cv.map(elem => elemT.render(context)(elem))
    Wikitext(renderedElems map (_.internal) mkString("\n"))    
  }
  
  /**
   * Also required for all Collections -- the default value to fall back on.
   */
  protected def doDefault(elemT:pType):implType
  final def default(elemT:pType):PropValue = makePropValue(doDefault(elemT))
  
  /**
   * Convenience wrapper for creating in-code PropValues.
   */
  def wrap(elem:ElemValue):implType
  def makePropValue(cv:implType):PropValue
  def apply(elem:ElemValue):PropValue = makePropValue(wrap(elem))
  
  /**
   * Returns the head of the collection.
   * 
   * NOTE: this will throw an exception if you call it on an empty collection! It is the
   * equivalent of Option.get
   */
  final def first(v:PropValue):ElemValue = v.cv.head
  
  final def isEmpty(v:PropValue):Boolean = v.cv.isEmpty
  
  implicit def toIterable(v:implType):Iterable[ElemValue] = v.asInstanceOf[Iterable[ElemValue]]
}

/**
 * A null collection, whose sole purpose is to be the cType for the initial hardcoded Properties.
 * 
 * TBD: this is bloody dangerous, and we'll see how well it works. But we have nasty
 * chicken-and-egg problems otherwise -- every Thing has Properties, which have Collections,
 * which causes looping. In particular, we need a Collection for the initial PropValues
 * to point to.
 */
class BootstrapCollection extends Collection(systemOID, systemOID, systemOID, () => emptyProps) {
  type implType = List[ElemValue]

  def doDeserialize(ser:String, elemT:pType):implType =
    throw new Exception("Can't deserialize a bootstrap collection!")

  def doSerialize(v:implType, elemT:pType):String =
    throw new Exception("Can't deserialize a bootstrap collection!")

  def doRender(context:ContextBase)(v:implType, elemT:pType):Wikitext = {
    elemT.render(context)(v.head)
  }
  def doDefault(elemT:pType):implType = {
    List(elemT.default)
  }
  def wrap(elem:ElemValue):implType = List(elem)
  def makePropValue(cv:implType):PropValue = BootstrapPropValue(cv)
    
  private case class BootstrapPropValue(cv:implType) extends PropValue {
    type myCollection = BootstrapCollection
      
    val coll = BootstrapCollection.this
  }  
}
object BootstrapCollection extends BootstrapCollection {
  def bootProp(oid:OID, v:Any) = (oid -> makePropValue(wrap(ElemValue(v))))
}

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property[VT, -RT](
    i:OID, 
    s:OID, 
    m:OID, 
    val pType:PType[VT] with PTypeBuilder[VT, RT], 
    val cType:Collection, 
    pf:PropFetcher)
  extends Thing(i, s, m, Kind.Property, pf) {
  def default = {
    // TODO: add the concept of the default meta-property, so you can set it
    // on a prop-by-prop basis
    cType.default(pType)
  }
  def defaultPair:PropAndVal[VT] = PropAndVal(this, default)
  // EVIL but arguably necessary. This is where we are trying to confine the cast from something
  // we get out of the PropMap (which is a bit undertyped) to match the associated Property.
  def castVal(v:PropValue) = v.asInstanceOf[PropValue]
  def pair(v:PropValue) = PropAndVal(this, castVal(v))

  override lazy val props:PropMap = propFetcher() + 
		  CollectionProp(cType) +
		  TypeProp(pType)
  
  def render(context:ContextBase)(v:PropValue) = {
    cType.render(context)(v, pType)
  }
  def renderedDefault = render(EmptyContext)(default)
  
  def from(m:PropMap):PropValue = castVal(m(this))
  def fromOpt(m:PropMap):Option[PropValue] = m.get(this.id) map castVal
  
  /**
   * Convenience method to fetch the value of this property in this map.
   */
  def first(m:PropMap):VT = pType.get(cType.first(from(m)))
  def firstOpt(m:PropMap):Option[VT] = fromOpt(m) map cType.first map pType.get
  def first(v:PropValue):VT = pType.get(cType.first(v))
  
  def isEmpty(v:PropValue) = cType.isEmpty(v)

  def apply(raw:RT) = (this.id, cType(pType(raw)))
  
  def validate(str:String) = pType.validate(str)
  def fromUser(str:String) = cType(pType.fromUser(str))
  // TODO: this clearly isn't correct. How are we actually going to handle more complex types?
  def toUser(v:PropValue):String = {
    val cv = castVal(v)
    if (cType.isEmpty(cv))
      ""
    else
      pType.toUser(cType.first(cv))
  }
  
  def serialize(v:PropValue):String = v.serialize(pType)
  def deserialize(str:String):PropValue = cType.deserialize(str, pType)
  
  // TODO: this is wrong. More correctly, we have to go through the cType as well:
  def renderInput(state:SpaceState, currentValue:Option[String]):Html = pType.renderInput(this, state, currentValue)
}

object Property {
  def optTextProp(id:OID, text:String) = (id -> Optional(ElemValue(PlainText(text)))) 
  /**
   * Convenience methods for meta-Properties
   */
  def placeholderText(text:String) = optTextProp(PlaceholderTextOID, text)  
  def prompt(text:String) = optTextProp(PromptOID, text)
  
  implicit object PropNameOrdering extends Ordering[Property[_,_]] {
    def compare(a:Property[_,_], b:Property[_,_]) = {
      if (a == NameProp) {
        if (b == NameProp)
          0
        else
          // Name always displays first
          -1
      } else
        a.displayName compare b.displayName
    }
  }
  
  import collection.immutable.TreeMap
  
  type PropList = TreeMap[Property[_,_], Option[String]]
  object PropList {
    def apply(pairs:(Property[_,_], Option[String])*):PropList = {
      (TreeMap.empty[Property[_,_], Option[String]] /: pairs)((m, pair) => m + pair)
    }
    
    def from(thing:Thing)(implicit state:SpaceState):PropList = {
      (TreeMap.empty[Property[_,_], Option[String]] /: thing.props.keys) { (m, propId) =>
        val prop = state.prop(propId)
        val value = prop.from(thing.props)
        // TODO: in the long run, this can't be a simple string:
        val str = prop.toUser(value)
        m + (prop -> Some(str))
      }
    }
  }
}

/**
 * A convenient wrapper for passing a value around in a way that can be fetched.
 */
case class PropAndVal[VT](prop:Property[VT, _], v:PropValue) {
  def render(context:ContextBase) = prop.render(context)(v)
  def renderPlain = render(EmptyContext)
  def renderOr(context:ContextBase)(other: => Wikitext) = if (prop.isEmpty(v)) other else render(context)
  def renderPlainOr(other: => Wikitext) = renderOr(EmptyContext)(other)
  def renderPlainIfDefined = if (!prop.isEmpty(v)) renderPlain else Wikitext("")
  def split() = (prop, v)
  def first = prop.first(v)
}

