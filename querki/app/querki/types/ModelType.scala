package querki.types

import models.{OID, Property, PropertyBundle, PType, PTypeBuilder, Thing, Wikitext}
import models.Thing.{PropMap, emptyProps}

import querki.ecology._
import querki.values.{PropAndVal, QLContext, QValue, SpaceState}

case class SimplePropertyBundle(props:PropMap)

object SimplePropertyBundle {
  def apply(vals:(OID, QValue)*):SimplePropertyBundle = {
    SimplePropertyBundle(emptyProps ++ vals)
  }
}

/**
 * TODO: a good deal of this code is copied from Thing. Think carefully about the right factoring here. I kind of
 * want PropertyBundle to remain a pure interface, but we may want to carefully lift out a base implementation.
 */
case class ModeledPropertyBundle(modelType:ModelTypeDefiner#ModelType, props:PropMap) extends PropertyBundle {
  def getModelOpt(implicit state:SpaceState):Option[Thing] = {
    state.anything(modelType.basedOn)
  }
  
  def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
    props.contains(propId) || { 
      getModelOpt.map(_.hasProp(propId)).getOrElse(false)
    }
  }
  
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = {
    if (hasProp(prop))
      Some(getProp(prop))
    else
      None
  }
  
  def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):QValue = {
    prop.fromOpt(props).getOrElse {
      getModelOpt.map(_.getPropVal(prop)).getOrElse { 
        modelType.interface[querki.ql.QL].WarningValue(s"Couldn't find Property ${prop.displayName} on the received value")
      }
    }
  }

  def localProp[VT, CT](prop:Property[VT, _]):Option[PropAndVal[VT]] = {
    prop.fromOpt(this.props) map prop.pair
  }
  
  def getProp[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    localProp(prop).getOrElse(getModelOpt.map(_.getProp(prop)).getOrElse(prop.defaultPair))
  }
  
  def map[VT, DT, RT](prop:Property[VT, _], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT)(implicit state:SpaceState):QValue = {
    val propAndVal = getProp(prop)
    propAndVal.map(destType)(cb)    
  }
}

trait ModelTypeDefiner { self:EcologyMember =>
  
  class ModelType(tid:OID, val basedOn:OID, typeProps:() => PropMap) extends querki.core.TypeUtils.SystemType[ModeledPropertyBundle](tid,
      typeProps) with PTypeBuilder[ModeledPropertyBundle, SimplePropertyBundle]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDeserialize") }
    def doSerialize(v:ModeledPropertyBundle)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doSerialize") }
    
    /**
     * By and large, we don't recommend simply displaying a Model Type, since the results are a bit unpredictable. But it should at
     * least be possible to do so.
     * 
     * TODO: in principle, it should be possible to use __stuff__ syntax to define how you want this to render. This is actually
     * hampered by the fact that we've already wikitexted it by this point. Should we be passing the AST into here instead of the
     * wikitext?
     */
    def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None) = {
      implicit val state = context.state
      // Introduce a bit of indirection, so we can sort the properties by display name:
      val propInfo = v.props.map { pair =>
        val (propId, propVal) = pair
        (propId, state.anything(propId), propVal)
      }
      val sortedInfos = propInfo.toSeq.sortBy(_._2.map(_.displayName).getOrElse(""))
      (Wikitext.empty /: propInfo) { (current, pair) =>
        val (propId, propOpt, propVal) = pair
        val propText = propOpt match {
          case Some(prop) => {
            Wikitext(": " + prop.displayName + " : ") + propVal.wikify(context, displayOpt)
          }
          case None => Wikitext("Unknown property " + propId)
        }
        current.+(propText, true)
      }
    }
    
    def doDefault(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDefault") }
    
    def wrap(raw:SimplePropertyBundle):ModeledPropertyBundle = {
      ModeledPropertyBundle(this, raw.props)
    }
  }
  
}