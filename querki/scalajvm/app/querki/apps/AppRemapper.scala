package querki.apps

import models._
import Thing._
import querki.cluster.OIDAllocator._
import querki.globals._
import querki.spaces.RTCAble
import querki.types.ModelTypeDefiner

/**
 * This single-function trait deals with computing the OIDs for a newly extracted App.
 * 
 * IMPORTANT: this function is *not* general! It only remaps the Models, not anything else.
 * Instances that have been lifted into the App will be deleted; Properties get *duplicated*
 * with the same OID in the App as in the child Space (to avoid the chaos of trying to deal
 * with multiple OIDs meaning essentially the same Property); and Model Types also get
 * duplicated, save that they get repointed to the new parent Model. This is very different
 * from importing a Space from a file, which wants to remap *all* of the OIDs.
 */
trait AppRemapper[RM[_]] extends EcologyMember with ModelTypeDefiner {
  private lazy val Core = interface[querki.core.Core]
  
  private lazy val LinkType = Core.LinkType
  
  implicit def rtc:RTCAble[RM]
  private implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  def extractorSupport:AppExtractorSupport[RM]
  
  /**
   * Given a proto-Space, this returns that State with all of the OIDs replaced by new ones.
   */
  def remapOIDs(state:SpaceState, includeSpace:Boolean):RM[(SpaceState, Map[OID, OID])] = 
  {
    val allThings = state.everythingLocal.toSeq
      
    // First, fetch new OIDs for the Models:
    val models = state.things.values.filter(_.isModel(state))
    extractorSupport.getOIDs(models.size).map { oids =>
      val pairs = models.map(_.id).zip(oids).toSeq
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
  
  /**
   * Remap any Links in this PropMap, if appropriate.
   */
  private def remapPropMap(t:Thing, state:SpaceState)(implicit oidMap:Map[OID, OID]):PropMap = {
    (t.props /: t.props) { case (props, (propId, propVal)) =>
      val propOpt = state.prop(propId)
      propOpt match {
        // TODO: in principle, we also need to catch Model Types here, and run through their
        // Property Bundles. In practice, it's an edge case of an edge case, so I'm not going
        // to worry about it quite yet.
        case Some(prop) if (prop.pType == LinkType) => {
          val rawOIDs = propVal.rawList(LinkType)
          val newElemValues = rawOIDs.map(mappedOID(_)).map(LinkType(_))
          val newVal = prop.cType.makePropValue(newElemValues, LinkType)
          props.updated(propId, newVal)
        }
        case _ => props
      }
    }
  }
  
  private def remapTypes(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.types.values) { (curState, typ) =>
      typ match {
        case mt:ModelTypeDefiner#ModelType => {
          val newType = ModelType(
            mt.id,
            mappedOID(curState.id),
            mt.mId,
            mappedOID(mt.basedOn),
            remapPropMap(mt, curState)
          )
          val newTypes = curState.types.updated(mt.id, newType)
          curState.copy(types = newTypes)
        }
        case _ => curState
      }
    }
  }
  
  private def remapProps(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.spaceProps.values) { (curState, prop) =>
      val newProp = prop.copy(
        s = mappedOID(curState.id),
        pType = curState.typ(mappedOID(prop.pType.id)).asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]],
        pf = remapPropMap(prop, curState)
      )
      val newProps = curState.spaceProps.updated(prop.id, newProp)
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
      // Note that we've got a new OID, so we can't just use curState.things.updated():
      val newThings = (curState.things - t.id) + (newId -> newThing)
      curState.copy(things = newThings)
    }
  }
}
