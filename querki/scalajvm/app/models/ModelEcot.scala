package models

import querki.ecology._
import querki.globals._
import querki.identity.IdentityId
import querki.persistence._
import querki.time.DateTime
import querki.types.ModelTypeDefiner

import Thing.PropMap

/**
 * The Ecot for the Models. This is mainly responsible for dealing with serialization.
 */
class ModelEcot(e:Ecology) extends QuerkiEcot(e) {
  import ModelPersistence._
  
  override def persistentMessages = persist(64,
    (classOf[DHPropMap] -> 100),
    (classOf[DHThingState] -> 101),
    (classOf[DHProperty] -> 102),
    (classOf[DHModelType] -> 103),
    (classOf[DHSpaceState] -> 104)
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
  
  def dh(ts:ThingState)(implicit state:SpaceState):DHThingState = DHThingState(ts.id, ts.model, ts.props, ts.modTime)
  def dh(prop:AnyProp)(implicit state:SpaceState):DHProperty = DHProperty(prop.id, prop.model, prop.props, prop.modTime, prop.pType.id, prop.cType.id)
  def dh(tpe:PType[_])(implicit state:SpaceState):DHModelType = {
    tpe match {
      case mt:ModelTypeDefiner#ModelType => DHModelType(mt.id, mt.model, mt.props, mt.modTime, mt.basedOn)
      case _ => throw new Exception(s"Trying to dehydrate PType $tpe, which isn't a ModelType!")
    }
  }
  def dh(state:SpaceState):DHSpaceState = {
    implicit val s = state
    
    DHSpaceState(
      state.id,
      state.model,
      state.props,
      state.modTime,
      state.owner,
      state.name,
      state.apps.map(_.id).toList,
      state.types.values.map(dh(_)).toList,
      state.spaceProps.values.map(dh(_)).toList,
      state.things.values.map(dh(_)).toList
    )
  }
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
  
  /**
   * A dehydrated Property. Strictly speaking we don't need to pType and cType -- they should be in the
   * props -- but it's convenient to keep them handy.
   */
  case class DHProperty(
    @KryoTag(1) id:OID, 
    @KryoTag(2) model:OID, 
    @KryoTag(3) props:DHPropMap, 
    @KryoTag(4) modTime:DateTime,
    @KryoTag(5) pType:OID,
    @KryoTag(6) cType:OID
  ) extends UseKryo
  
  /**
   * A dehydrated ModelType. (Which so far are the only user-createable PTypes, and might always be.)
   * The basedOn field is redundant -- it should be in the props -- but it's convenient to have it out.
   * 
   * This arguably doesn't belong here, but it's convenient.
   */
  case class DHModelType(
    @KryoTag(1) id:OID, 
    @KryoTag(2) model:OID, 
    @KryoTag(3) props:DHPropMap, 
    @KryoTag(4) modTime:DateTime,
    @KryoTag(5) basedOn:OID
  ) extends UseKryo
  
  /**
   * A dehydrated SpaceState.
   * 
   * Note that we aren't even bothering to dehydrate Collection yet, since there is no concept of
   * user-defined ones yet.
   */
  case class DHSpaceState(
    @KryoTag(1) id:OID, 
    @KryoTag(2) model:OID, 
    @KryoTag(3) props:DHPropMap, 
    @KryoTag(4) modTime:DateTime,
    @KryoTag(5) ownerId:IdentityId,
    @KryoTag(6) name:String,
    @KryoTag(7) apps:List[OID],
    @KryoTag(8) types:List[DHModelType],
    @KryoTag(9) spaceProps:List[DHProperty],
    @KryoTag(10) things:List[DHThingState]
  ) extends UseKryo
}
