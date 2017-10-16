package models

import querki.globals._
import querki.ql.Invocation
import querki.values.{QFut, RequestContext}

class PropertyThingOps[VT,RT](prop:Property[VT,RT])(implicit e:Ecology) extends ThingOps(prop) {
  
  def pType = prop.pType

  /**
   * This renders the Property itself, if it has no DisplayText defined.
   */
  override def renderDefault(implicit request:RequestContext, state:SpaceState):Future[Wikitext] = {
    val fromType = pType.renderProperty(prop)
    fromType.getOrElse(renderProps)
  }
  
  def checkForParams(inv:Invocation):Option[QFut] = {
    if (inv.paramsOpt.isDefined && 
        !((prop.pType.id == querki.core.MOIDs.InternalMethodOID) ||
          (prop.pType.id == querki.basic.MOIDs.QLTypeOID))) 
    {
      Some(e.api[ModelInternal].buildPropVal(prop, inv))
    } else
      None
  }
  
  /**
   * By default, qlApply on a Property expects the input context to be a single Link. It returns the value
   * of this Property on that Link.
   * 
   * TODO: if this Property isn't defined on the target Thing or its ancestors, this should return None.
   * So technically, this should be returning Optional. Note that PType.qlApply() already does this.
   */
  override def qlApply(inv:Invocation):QFut = {
    // If the property has parameters, then it is a Property Value constructor, rather than
    // an accessor:
    (checkForParams(inv)
      // Give the Type first dibs at handling the call; otherwise, return the value of this property
      // on the incoming thing.
      .getOrElse(pType.qlApplyFromProp(inv, prop)
      .getOrElse(prop.applyToIncomingProps(inv) { (t, innerContext) =>
          t.getPropVal(prop)(innerContext.state)
        })))
  }  
}
