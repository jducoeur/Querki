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
  
  /**
   * By default, qlApply on a Property expects the input context to be a single Link. It returns the value
   * of this Property on that Link.
   * 
   * TODO: if this Property isn't defined on the target Thing or its ancestors, this should return None.
   * So technically, this should be returning Optional. Note that PType.qlApply() already does this.
   */
  override def qlApply(inv:Invocation):QFut = {
    pType.qlApplyFromProp(inv, prop)
      .getOrElse(prop.applyToIncomingProps(inv) { (t, innerContext) =>
        t.getPropVal(prop)(innerContext.state)
      })
  }  
}
