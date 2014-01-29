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
    
    def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None) = { throw new Exception("WrappedValueType does not implement doWikify") }
    
    def doDefault(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDefault") }
    
    def wrap(raw:SimplePropertyBundle):ModeledPropertyBundle = {
      ModeledPropertyBundle(this, raw.props)
    }
  }
  
}