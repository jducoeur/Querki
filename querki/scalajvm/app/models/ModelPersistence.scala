package models

import querki.ecology._
import querki.globals._
import querki.identity.IdentityId
import querki.persistence._
import querki.spaces.UnresolvedPropValue
import querki.time.DateTime
import querki.types.ModelTypeDefiner
import querki.values.{SpaceState, SpaceVersion}

import Thing.PropMap

trait ModelPersistence { self:EcologyMember with querki.types.ModelTypeDefiner =>
  
  import ModelPersistence._
  
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  
  def recordUnresolvedProp(valStr:String) = interface[querki.spaces.SpacePersistence].recordUnresolvedProp(valStr)
  lazy val systemState = interface[querki.system.System].State
  lazy val UnresolvedPropType = interface[querki.spaces.SpacePersistence].UnresolvedPropType
  
  /**
   * This is the "magic string" that means that this Property value has been deleted.
   */
  final val DeletedValueSignal = "\uFFFD"
  
  implicit def propMap2DH(pm:PropMap)(implicit state:SpaceState):DHPropMap = {
    val props = pm.map { case (k,v) =>
      if (v.isDeleted)
        (k, DeletedValueSignal)
      else state.prop(k) match {
        case Some(prop) => {
          (k, prop.serialize(v))
        }
        case None => {
          QLog.stackTrace(s"Trying to serialize unknown Property $k, with value $v!")
          (k, "")
        }
      }
    }
    DHPropMap(props)
  }
  
  implicit def DH2PropMap(dh:DHPropMap)(implicit state:SpaceState):PropMap = {
    dh.props.map { case(k,v) =>
      state.prop(k) match {
        case Some(prop) => {
          if (v == DeletedValueSignal)
            (k, DataModelAccess.getDeletedValue(prop))
          else
            (k, prop.deserialize(v))
        }
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
      state.apps.map(app => (app.id, app.version.v)).toList,
      state.types.values.map(dh(_)).toList,
      state.spaceProps.values.map(dh(_)).toList,
      state.things.values.map(dh(_)).toList
    )
  }
  
  /**
   * This deals with the synchronous bits of "rehydrating" a SpaceState. Note that the end result is still
   * missing a few bits that require Actor communication: the apps and the ownerIdentity.
   * 
   * Note that this code is intentionally adapted from SpaceLoader, which it will eventually replace.
   */
  def rehydrate(dh:DHSpaceState):SpaceState = {
    // First, create the framework of the SpaceState itself:
    val baseState = 
      SpaceState(
        dh.id,
        dh.model,
        DH2PropMap(dh.props)(systemState),
        dh.ownerId,
        dh.name,
        dh.modTime,
        Seq.empty,  // TODO: apps need to be filled in async
        Some(systemState),
        Map.empty,  // Types -- filled in below
        Map.empty,  // SpaceProps -- filled in below
        Map.empty,  // Things -- filled in below
        Map.empty,  // Collections -- ignored for now
        None,       // ownerIdentity needs to be filled in async
        appInfo = dh.apps.map { case (id, l) => (id, SpaceVersion(l)) }
      )
      
    // Next, add the Types. Note that we can build the Type before we build the
    // Model it is based on.
    val typeMap = (Map.empty[OID, PType[_]] /: dh.types) { (map, tpe) =>
      implicit val s = baseState
      // TODO: ModelType should take modTime, like all other dynamically-created Things:
      val mt = new ModelType(tpe.id, dh.id, querki.core.MOIDs.UrTypeOID, tpe.basedOn, tpe.props)
      map + (tpe.id -> mt)
    }
    val withTypes = baseState.copy(types = typeMap)
    
    // Next, add the Properties.
    val props = (Map.empty[OID, Property[_,_]] /: dh.spaceProps) { (map, propdh) =>
      implicit val s = withTypes
      val typ = withTypes.typ(propdh.pType)
      // This sad cast is necessary in order to pass the PType into the Property. It's reasonably
      // safe: we don't actually care about the type parameters of user-created Properties, since
      // erasure is going to scrag them anyway:
      val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
      val coll = systemState.coll(propdh.cType)
      val prop = new Property(propdh.id, dh.id, propdh.model, boundTyp, coll, propdh.props, propdh.modTime)
      map + (propdh.id -> prop)
    }
    val withProps = withTypes.copy(spaceProps = props)
    
    // Now add the Things.
    val ts = (Map.empty[OID, ThingState] /: dh.things) { (map, thingdh) =>
      implicit val s = withProps
      val thing = ThingState(thingdh.id, dh.id, thingdh.model, thingdh.props, thingdh.modTime)
      map + (thingdh.id -> thing)
    }
    val withThings = withProps.copy(things = ts)
    
    // Now we do a second pass, to resolve anything left unresolved:
    def secondPassProps[T <: Thing](thing:T)(copier:(T, PropMap) => T):T = {
      val fixedProps = thing.props.map { propPair =>
        val (id, value) = propPair
        value match {
          case unres:UnresolvedPropValue => {
            val propOpt = withThings.prop(id)
            val v = propOpt match {
              case Some(prop) => prop.deserialize(value.firstTyped(UnresolvedPropType).get)(withThings)
              case None => value
            }
            (id, v)
          }
          case _ => propPair
        }
      }
      copier(thing, fixedProps)
    }
    // First fix up the Space itself:
    val fixedSpace = secondPassProps(withThings)((state, props) => state.copy(pf = props))
    // And then the Properties:
    val fixedProps = fixedSpace.spaceProps.mapValues { prop =>
      secondPassProps(prop)((p, metaProps) => p.copy(pf = metaProps))
    }
    fixedSpace.copy(spaceProps = fixedProps)
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
    @KryoTag(7) apps:List[(OID, Long)],
    @KryoTag(8) types:List[DHModelType],
    @KryoTag(9) spaceProps:List[DHProperty],
    @KryoTag(10) things:List[DHThingState]
  ) extends UseKryo
}
