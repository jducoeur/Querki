package models

import querki.ecology._
import querki.globals._
import querki.persistence._

import Thing.PropMap

/**
 * The Ecot for the Models. This is mainly responsible for dealing with serialization.
 */
class ModelEcot(e:Ecology) extends QuerkiEcot(e) {
  import ModelPersistence._
  
  override def persistentMessages = persist(64,
    (classOf[DHPropMap] -> 100)
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
}

object ModelPersistence {
  /**
   * A "dehydrated" PropMap. This is the pre-serialized form. We have to do this in a separate step,
   * because dehydrate/hydrate require a SpaceState, which isn't available at deserialization time.
   */
  case class DHPropMap(@KryoTag(1) props:Map[OID,String]) extends UseKryo
}
