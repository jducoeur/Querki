package models

import language.existentials

import Thing._

import querki.core.MOIDs._
import querki.ecology._
import querki.ql.{Invocation, PartiallyAppliedFunction, QLFunction, QLPhrase}
import querki.time.DateTime
import querki.types.Types

import querki.util.QLog
import querki.values._

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property[VT, RT](
    i:OID, 
    s:OID, 
    m:OID, 
    val pType:PType[VT] with PTypeBuilder[VT, RT], 
    val cType:Collection, 
    pf:PropFetcher,
    mt:DateTime)(implicit e:Ecology) 
  extends Thing(i, s, m, Kind.Property, pf, mt)(e)
{
  def Core = interface[querki.core.Core]
  
  lazy val DefaultValueProp = interface[Types].DefaultValueProp
  def WarningValue(msg:String) = interface[querki.ql.QL].WarningValue(msg)
  def ErrorValue(msg:String) = interface[querki.ql.QL].ErrorValue(msg)
    
  def default(implicit state:SpaceState) = {
    val explicitDefault = localProp(DefaultValueProp).map(_.v)
    explicitDefault.getOrElse(cType.default(pType))
  }
  def defaultPair(implicit state:SpaceState):PropAndVal[VT] = PropAndVal(this, default)
  def pair(v:QValue) = PropAndVal(this, v)

  override lazy val props:PropMap = propFetcher() + 
		  (CollectionPropOID -> Core.ExactlyOne(Core.LinkType(cType))) +
		  (TypePropOID -> Core.ExactlyOne(Core.LinkType(pType)))

  /**
   * This little method is a type-math workaround. We're often dealing with properties in contexts
   * where we know the PType of the property (and thus, the VT) for external reasons, but the
   * Scala compiler isn't smart enough to figure that out. So this provides a consistent way to
   * do the cast safely, at runtime. 
   */
  def confirmType[PVT](pt:PType[PVT]):Option[Property[PVT,_]] = {    
    if (pt == pType)
      Some(this.asInstanceOf[Property[PVT,_]])
    else
      None
  }
  
  def from(m:PropMap):QValue = m(this)
  def fromOpt(m:PropMap):Option[QValue] = m.get(this.id)
  
  /**
   * Convenience method to fetch the value of this property in this map.
   */
  def first(m:PropMap):VT = pType.get(cType.first(from(m)))
  def firstOpt(m:PropMap):Option[VT] = fromOpt(m) map cType.first map pType.get

  def apply(raws:RT*) = (this.id, QValue.make(cType, pType, raws:_*))
  def apply(qv:QValue) = (this.id, qv)
  
  def validate(str:String, state:SpaceState) = pType.validate(str, this, state)
  
  def validatingQValue[R](v:QValue)(f: => R):R = {
    if (v.cType != cType)
      QLog.error(s"Property $displayName Validation Failed: expected collection ${cType.displayName}, but got ${v.cType.displayName}")
    if (v.pType != pType)
      QLog.error(s"Property $displayName Validation Failed: expected type ${pType.displayName}, but got ${v.pType.displayName}")
      
    f
  }
  
  // TODO: this clearly isn't correct. How are we actually going to handle more complex types?
  def toUser(v:QValue)(implicit state:SpaceState):String = {
    if (cType.isEmpty(v))
      ""
    else
      pType.toUser(cType.first(v))
  }
  
  /**
   * This is a deliberately hardcoded and SpaceState-less lookup of whether this Property is NotInherited. It is
   * designed to be as fast as possible, because getPropOpt depends on it. (Which is why it doesn't use a SpaceState;
   * that way, it can be a lazy val.)
   * 
   * TBD: this explicitly assumes that we don't have Not Inherited Properties descending from each other. That's
   * a potentially questionable assumption in principle, but until we have mechanisms for Property inheritance, it'll do.
   */
  lazy val notInherited:Boolean = {
    if (props.contains(querki.core.MOIDs.NotInheritedOID)) {
      // EEEEVIL! Do not imitate this code, which makes all sorts of horrible assumptions that do not
      // generalize! This is designed to be fast, not correct.
      props(querki.core.MOIDs.NotInheritedOID).first.elem.asInstanceOf[Boolean]
    } else
      false
  }
  
  def serialize(v:QValue)(implicit state:SpaceState):String = validatingQValue(v){ v.serialize(pType) }
  def deserialize(str:String)(implicit state:SpaceState):QValue = cType.deserialize(str, pType)
  
  // TODO: these two methods are probably obsolete. Can they consistently be replaced by
  // inv.bundlesAndContextsForProp?
  def applyToIncomingThing(inv:Invocation)(action:(Thing, QLContext) => QValue):QValue = {
    applyToIncomingProps(inv) { (props, internalContext) =>
      props match {
        case t:Thing => action(t, internalContext)
        case _ => ErrorValue("Got a PropertyBundle where we we expected a Thing -- there is probably a call to applyToIncomingThing that should be Props")
      }
    }
  }
  
  def applyToIncomingProps(inv:Invocation)(action:(PropertyBundle, QLContext) => QValue):QValue = {
    for {
      (bundle, elemContext) <- inv.bundlesAndContextsForProp(this)
    }
      yield action(bundle, elemContext)
  }
  
  override def thingOps(ecology:Ecology):ThingOps = new PropertyThingOps(this)(ecology)
}

class PropertyThingOps[VT,RT](prop:Property[VT,RT])(implicit e:Ecology) extends ThingOps(prop) {
  
  def pType = prop.pType

  /**
   * This renders the Property itself, if it has no DisplayText defined.
   */
  override def renderDefault(implicit request:RequestContext, state:SpaceState):Wikitext = {
    val fromType = pType.renderProperty(prop)
    fromType.getOrElse(renderProps)
  }
  
  /**
   * By default, qlApply on a Property expects the input context to be a single Link. It returns the value
   * of this Property on that Link.
   * 
   * TODO: if this Property isn't defined on the target Thing or its ancestors, this should return None.
   * So technically, this should be returning Optional. Note that PType.qlApply() already does this.
   */
  override def qlApply(inv:Invocation):QValue = {
    // Give the Type first dibs at handling the call; otherwise, return the value of this property
    // on the incoming thing.
    pType.qlApplyFromProp(inv, prop).getOrElse(
      prop.applyToIncomingProps(inv) { (t, innerContext) =>
        t.getPropVal(prop)(innerContext.state)
      })
  }  
  
  override def partiallyApply(leftContext:QLContext):QLFunction = {
    def handleRemainder(inv:Invocation):QValue = {
      qlApply(inv)
//      // Note that partial application ignores the incoming context if the type isn't doing anything clever. By
//      // and large, this syntax mainly exists for QL properties:
//      //
//      //   incomingContext -> definingThing.MyQLFunction(params)
//      //
//      // But we allow you to use partial application in general, since it sometimes feels natural.
//      pType.qlApplyFromProp(leftContext, inv.context, this, inv.paramsOpt).getOrElse(applyToIncomingThing(leftContext) { (t, context) =>
//        t.getPropVal(this)(context.state)
//      })
    }
    new PartiallyAppliedFunction(leftContext, handleRemainder)
  }
  
}