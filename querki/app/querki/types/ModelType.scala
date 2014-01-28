package querki.types

import models.{OID, SimplePTypeBuilder, Wikitext}
import models.Thing.PropMap

import querki.ecology._
import querki.values.{QLContext, SpaceState}

trait ModelTypeDefiner { self:EcologyMember =>
  
  class ModelType(tid:OID, basedOn:OID, typeProps:() => PropMap) extends querki.core.TypeUtils.SystemType[PropMap](tid,
      typeProps) with SimplePTypeBuilder[PropMap]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDeserialize") }
    def doSerialize(v:PropMap)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doSerialize") }
    
    def doWikify(context:QLContext)(v:PropMap, displayOpt:Option[Wikitext] = None) = { throw new Exception("WrappedValueType does not implement doWikify") }
    
    def doDefault(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDefault") }
  }
  
}