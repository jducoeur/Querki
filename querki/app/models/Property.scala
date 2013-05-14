package models

import language.existentials
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
    v.render(context, pType)
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
  
  def flatMap[T](v:PropValue)(cb:VT => Option[T]) = v.cv.flatMap { elem => 
    val vt = pType.get(elem)
    cb(vt)
  }
  
  def contains(v:PropValue, toCheck:VT):Boolean = v.cv.exists { elem =>
    val vt = pType.get(elem)
    vt == toCheck
  }

  def apply(raw:RT) = (this.id, cType(pType(raw)))
  def apply() = (this.id, cType.default(pType))
  
  def validate(str:String) = pType.validate(str)
  import play.api.data.Form
  def fromUser(form:Form[_]):FormFieldInfo = cType.fromUser(form, this, pType)
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
  
  def renderInput(state:SpaceState, currentValue:DisplayPropVal):Html = {
    Html(cType.renderInput(this, state, currentValue, pType).toString)
  }
  
  
  def applyToIncomingThing(context:ContextBase)(action:(Thing, ContextBase) => TypedValue):TypedValue = {
    val valType = context.value.pt
    valType match {
      case link:LinkType => {
        val coll = context.value.ct
        val thing = link.followLink(context)
        thing match {
          case Some(t) => action(t, context)
          case None => ErrorValue("Couldn't find Thing from " + context.toString)
        }
      }
      case _ => ErrorValue("Can't apply a Property in a " + valType.displayName + " context!")
    }    
  }
  
  /**
   * qlApply on a Property expects the input context to be a single Link. It returns the value
   * of this Property on that Link.
   * 
   * TODO: if this Property isn't defined on the target Thing or its ancestors, this should return None.
   * So technically, this should be returning Optional.
   */
  override def qlApply(context:ContextBase, params:Option[Seq[ContextBase]] = None):TypedValue = {
    applyToIncomingThing(context) { (t, context) =>
      val result = t.getPropVal(this)(context.state)
      TypedValue(result, pType)
    }
  }  
}

case class DisplayPropVal(prop: Property[_,_], v: Option[PropValue], inheritedVal:Option[PropValue] = None, inheritedFrom:Option[Thing] = None) {
  def isInherited = v.isEmpty && inheritedVal.isDefined
  def propId = prop.id.toString
  def inputControlId = "v-" + propId
  def collectionControlId = "coll-" + propId
  // This is a hidden input field, which is a flag to tell the receiving code whether the
  // field is "empty" -- inherited or deleted, but with no local value:
  def emptyControlId = "empty-" + propId
  def hasInheritance = inheritedVal.isDefined
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
      if (a eq NameProp) {
        if (b eq NameProp)
          0
        else
          // Name always displays first
          -1
      } else if (b eq NameProp)
        1
      else
        a.displayName compare b.displayName
    }
  }
  
  import collection.immutable.TreeMap
  
  type PropList = TreeMap[Property[_,_], DisplayPropVal]
  object PropList {
    def apply(pairs:(Property[_,_], DisplayPropVal)*):PropList = {
      (TreeMap.empty[Property[_,_], DisplayPropVal] /: pairs)((m, pair) => m + pair)
    }
    
    def empties(props:Property[_,_]*):PropList = {
      val pairs = props map (prop => (prop, DisplayPropVal(prop, None)))
      apply(pairs:_*)
    }
    
    def inheritedProps(model:Thing)(implicit state:SpaceState):PropList = {
      // Get the Model's PropList, and push its values into the inherited slots:
      val raw = from(model)
      raw map { fromModel =>
        val (prop, v) = fromModel
        if (prop.first(NotInheritedProp))
          (prop, DisplayPropVal(prop, None))
        else if (v.v.isDefined)
          (prop, DisplayPropVal(prop, None, v.v, Some(model)))
        else
          fromModel
      }      
    }
    
    // TODO: this is all pretty inefficient. We should be caching the PropLists,
    // especially for common models.
    def from(thing:Thing)(implicit state:SpaceState):PropList = {
      val inherited =
        if (thing.hasModel)
          inheritedProps(thing.getModel)
        else
          TreeMap.empty[Property[_,_], DisplayPropVal]      
      
      (inherited /: thing.props.keys) { (m, propId) =>
        val prop = state.prop(propId)
        val value = prop.from(thing.props)
        val disp =
          if (m.contains(prop))
            m(prop).copy(v = Some(value))
          else
            DisplayPropVal(prop, Some(value))
        m + (prop -> disp)
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
  def flatMap[T](cb:VT => Option[T]) = prop.flatMap(v)(cb)
  def ++(others:Iterable[VT]):PropValue = {
    QList.makePropValue((v.cv ++ others.map(ElemValue(_))).toList)
  }
  def contains(toCheck:VT):Boolean = prop.contains(v, toCheck)
}

