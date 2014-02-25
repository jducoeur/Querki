package models

import language.existentials

import Thing.{PropFetcher, PropMap}

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
case class Property[VT, -RT](
    i:OID, 
    s:OID, 
    m:OID, 
    val pType:PType[VT] with PTypeBuilder[VT, RT], 
    val cType:Collection, 
    pf:PropFetcher,
    mt:DateTime)(implicit e:Ecology) 
  extends Thing(i, s, m, Kind.Property, pf, mt)(e)
{
  lazy val DefaultValueProp = interface[Types].DefaultValueProp
  def WarningValue(msg:String) = interface[querki.ql.QL].WarningValue(msg)
  def ErrorValue(msg:String) = interface[querki.ql.QL].ErrorValue(msg)
    
  def default(implicit state:SpaceState) = {
    val explicitDefault = localProp(DefaultValueProp).map(_.v)
    explicitDefault.getOrElse(cType.default(pType))
  }
  def defaultPair(implicit state:SpaceState):PropAndVal[VT] = PropAndVal(this, default)
  // EVIL but arguably necessary. This is where we are trying to confine the cast from something
  // we get out of the PropMap (which is a bit undertyped) to match the associated Property.
  def castVal(v:QValue) = v.asInstanceOf[QValue]
  def pair(v:QValue) = PropAndVal(this, castVal(v))

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
  
  /**
   * This renders the Property itself, if it has no DisplayText defined.
   */
  override def renderDefault(implicit request:RequestContext):Wikitext = {
    val fromType = pType.renderProperty(this)
    fromType.getOrElse(renderProps)
  }
  
  def from(m:PropMap):QValue = castVal(m(this))
  def fromOpt(m:PropMap):Option[QValue] = m.get(this.id) map castVal
  
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
    val cv = castVal(v)
    if (cType.isEmpty(cv))
      ""
    else
      pType.toUser(cType.first(cv))
  }
  
  def serialize(v:QValue)(implicit state:SpaceState):String = validatingQValue(v){ v.serialize(pType) }
  def deserialize(str:String)(implicit state:SpaceState):QValue = cType.deserialize(str, pType)
  
  // TODO: these two methods are probably obsolete. Can they consistently be replaced by
  // inv.contextBundlesAndContexts?
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
      (bundle, elemContext) <- inv.contextBundlesAndContexts
    }
      yield action(bundle, elemContext)
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
    pType.qlApplyFromProp(inv, this).getOrElse(
      applyToIncomingProps(inv) { (t, innerContext) =>
        t.getPropVal(this)(innerContext.state)
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
