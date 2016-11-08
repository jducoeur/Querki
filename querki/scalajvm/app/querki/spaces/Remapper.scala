package querki.spaces

import org.querki.requester._

import models._
import Thing._
import querki.cluster.OIDAllocator._
import querki.globals._
import querki.types.ModelTypeDefiner

/**
 * This single-function trait deals with computing the OIDs for a newly extracted or
 * imported Space.
 */
trait Remapper extends EcologyMember with RequesterImplicits with ModelTypeDefiner {
  private lazy val Cluster = interface[querki.cluster.QuerkiCluster]
  private lazy val Core = interface[querki.core.Core]
  
  private lazy val LinkType = Core.LinkType
  
  /**
   * Given a proto-Space, this returns that State with all of the OIDs replaced by new ones.
   */
  def remapOIDs(state:SpaceState, includeSpace:Boolean):RequestM[(SpaceState, Map[OID, OID])] = {
    val allThings = state.everythingLocal.toSeq
      
    // First, fetch new OIDs for everything:
    val allocator = Cluster.oidAllocator
    (allocator ? GiveOIDBlock(allThings.size)).map { case NewOIDs(oids) =>
      val pairs = allThings.map(_.id).zip(oids)
      // This is now a Map from old to new OIDs:
      implicit val oidMap = Map(pairs:_*)
      // IMPORTANT NOTE: the way this works, we're implicitly assuming that Types and Props don't
      // themselves have meta-Props whose *values* are pointing to other Things in this Space. That
      // may or may not be a safe assumption -- keep an eye on it! (The issue is that, until the
      // Props have been remapped, we're going to tend to fail to fetch Prop values.)
      val withTypes = remapTypes(state)
      val withProps = remapProps(withTypes)
      val withThings = remapThings(withProps)
      val withOwnProps =
        if (includeSpace)
          withThings.copy(pf = remapPropMap(withThings, withThings))
        else
          withThings
      (withOwnProps, oidMap)
    }
  }
  
  private def mappedOID(oid:OID)(implicit oidMap:Map[OID, OID]):OID = {
    oidMap.get(oid).getOrElse(oid)
  }
  
  private def remapPropMap(t:Thing, state:SpaceState)(implicit oidMap:Map[OID, OID]):PropMap = {
    (t.props /: t.props) { case (props, (propId, propVal)) =>
      val newId = mappedOID(propId)
      val propOpt = state.prop(propId)
      val newVal = propOpt.map { prop =>
        prop.pType match {
          case LinkType => {
            val rawOIDs = propVal.rawList(LinkType)
            val newElemValues = rawOIDs.map(mappedOID(_)).map(LinkType(_))
            prop.cType.makePropValue(newElemValues, LinkType)
          }
          // TODO: in principle, we also need to catch Model Types here, and run through their
          // Property Bundles. In practice, it's an edge case of an edge case, so I'm not going
          // to worry about it quite yet.
          case _ => propVal
        }
      }.getOrElse(propVal)
      (props - propId) + (newId -> newVal)
    }
  }
  
  private def remapTypes(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.types.values) { (curState, typ) =>
      typ match {
        case mt:ModelTypeDefiner#ModelType => {
          val newId = mappedOID(mt.id)
          val newType = ModelType(
            newId,
            mappedOID(curState.id),
            mappedOID(mt.mId),
            mappedOID(mt.basedOn),
            remapPropMap(mt, curState)
          )
          val newTypes = (curState.types - mt.id) + (newId -> newType)
          curState.copy(types = newTypes)
        }
        case _ => curState
      }
    }
  }
  
  private def remapProps(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.spaceProps.values) { (curState, prop) =>
      val newId = mappedOID(prop.id)
      val newProp = prop.copy(
        i = newId,
        s = mappedOID(curState.id),
        m = mappedOID(prop.model),
        pType = curState.typ(mappedOID(prop.pType.id)).asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]],
        pf = remapPropMap(prop, curState)
      )
      val newProps = (curState.spaceProps - prop.id) + (newId -> newProp)
      curState.copy(spaceProps = newProps)
    }
  }
  
  private def remapThings(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.things.values) { (curState, t) =>
      val newId = mappedOID(t.id)
      val newThing = t.copy(
        i = newId,
        s = mappedOID(curState.id), 
        m = mappedOID(t.model), 
        pf = remapPropMap(t, curState))
      val newThings = (curState.things - t.id) + (newId -> newThing)
      curState.copy(things = newThings)
    }
  }
}
