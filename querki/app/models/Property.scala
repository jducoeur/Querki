package models

import play.api.templates.Html

import Thing._

import system._
import system.OIDs._
import system.SystemSpace._

import ql._

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
  
  /**
   * qlApply on a Property expects the input context to be a single Link. It returns the value
   * of this Property on that Link.
   * 
   * TODO: if this Property isn't defined on the target Thing or its ancestors, this should return None.
   * So technically, this should be returning Optional.
   */
  override def qlApply(context:ContextBase):TypedValue = {
    val valType = context.value.pt
    valType match {
      case link:LinkType => {
        val coll = context.value.ct
        // TODO: Evil!
        val oid = context.value.v.first.elem.asInstanceOf[OID]
        val thing = link.follow(context)(oid)
        thing match {
          case Some(t) => {
            val result = t.getPropVal(this)(context.state)
            TypedValue(result, pType)
          }
          case None => ErrorValue("Couldn't find Thing " + oid.toString)
        }
      }
      case _ => ErrorValue("Can't apply a Property in a " + valType + " context.")
    }
  }  
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

