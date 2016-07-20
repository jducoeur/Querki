package models

import querki.ecology._
import querki.globals._
import querki.persistence._
import querki.time.DateTime

import Thing.PropMap

/**
 * The Ecot for the Models. This is mainly responsible for dealing with serialization.
 */
class ModelEcot(e:Ecology) extends QuerkiEcot(e) {
  import ModelPersistence._
  
  override def persistentMessages = persist(64,
    (classOf[DHPropMap] -> 100),
    (classOf[DHThingState] -> 101)
  )
}

trait ModelPersistence { self:EcologyMember =>
  
  import ModelPersistence._
  
  def recordUnresolvedProp(valStr:String) = interface[querki.spaces.SpacePersistence].recordUnresolvedProp(valStr)
  
  implicit def propMap2DH(pm:PropMap)(implicit state:SpaceState):DHPropMap = {
    val props = pm.map { case (k,v) =>
      state.prop(k) match {
        case Some(prop) => {
          (k, prop.serialize(v))
        }
        case None => {
          QLog.error(s"Trying to serialize unknown Property $k, with value $v!")
          (k, "")
        }
      }
    }
    DHPropMap(props)
  }
  
  implicit def DH2PropMap(dh:DHPropMap)(implicit state:SpaceState):PropMap = {
    dh.props.map { case(k,v) =>
      state.prop(k) match {
        case Some(prop) => (k, prop.deserialize(v))
        // We'll presume for now that this Property hasn't been deserialized itself yet:
        case None => (k, recordUnresolvedProp(v))
      }
    }
  }
  
  def dh(ts:ThingState)(implicit state:SpaceState) = DHThingState(ts.id, ts.model, ts.props, ts.modTime)
}

object ModelPersistence {
  /**
   * A "dehydrated" PropMap. This is the pre-serialized form. We have to do this in a separate step,
   * because dehydrate/hydrate require a SpaceState, which isn't available at deserialization time.
   */
  case class DHPropMap(@KryoTag(1) props:Map[OID,String]) extends UseKryo
  
  /**
   * A dehydrated ThingState.
   */
  case class DHThingState(@KryoTag(1) id:OID, @KryoTag(2) model:OID, @KryoTag(3) props:DHPropMap, @KryoTag(4) modTime:DateTime) extends UseKryo
}
