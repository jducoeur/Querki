package querki.imexport

import models._
import querki.cluster.OIDAllocator._
import querki.globals._
import querki.spaces.RTCAble
import querki.types.{ModelTypeBase, ModelTypeDefiner, SimplePropertyBundle}

/**
 * This single-function trait deals with computing the OIDs for a newly imported Space.
 * 
 * IMPORTANT: this is similar to the AppRemapper, but different in a bunch of crucial ways.
 * In particular, this needs to remap *all* of the IDs, where AppRemapper only does some of
 * them. We might want to merge the two at some point, but it will need to be done with
 * extreme care in order to not break things.
 * 
 * TODO: the handling on Model Types is probably too naive here. I suspect we need to do a
 * topological sort in case of Model Types that depend on other Model Types, and rebuild
 * them in order.
 */
trait Remapper[RM[_]] extends EcologyMember with ModelTypeDefiner {
  private lazy val Core = interface[querki.core.Core]
  
  private lazy val LinkType = Core.LinkType
  
  implicit def rtc:RTCAble[RM]
  private implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  def getOIDs(size:Int):RM[Seq[OID]]
  
  /**
   * Given a proto-App, this returns that State with all of the OIDs replaced by new ones.
   * Note that this already only contains the Things that are being extracted to the App.
   */
  def remapOIDs(stateOriginal:SpaceState):RM[(SpaceState, Map[OID, OID])] = 
  {
    val allThings = stateOriginal.everythingLocal.toSeq
      
    val oidsToMap = (stateOriginal.everythingLocal.toSeq :+ stateOriginal).map(_.id)
    getOIDs(oidsToMap.size).map { oids =>
      val pairs = oidsToMap.zip(oids)
       // This is now a Map from old to new OIDs:
      implicit val oidMap = Map(pairs:_*)
      // Note that "and" is magic syntax sugar on SpaceState that lets me chain without creating lots
      // of intermediate names:
      val transformed =
        remapTypes(stateOriginal)(stateOriginal) and
        remapProps(stateOriginal) and
        remapThings(stateOriginal) and
        { _.copy(s = oidMap(stateOriginal.id)) } and
        { withOwnID => withOwnID.copy(pf = remapPropMap(withOwnID, withOwnID, stateOriginal)) }
      (transformed, oidMap)
    }
  }
  
  private def mappedOID(oid:OID)(implicit oidMap:Map[OID, OID]):OID = {
    oidMap.get(oid).getOrElse(oid)
  }
  
  /**
   * Remap any Links in this PropMap, if appropriate.
   */
  private def remapPropMap(t:Thing, state:SpaceState, stateOriginal:SpaceState)(implicit oidMap:Map[OID, OID]):PropMap = {
    remapPropMap(t.props, state, stateOriginal)
  }
  
  private def remapPropMap(propsIn:PropMap, state:SpaceState, stateOriginal:SpaceState)(implicit oidMap:Map[OID, OID]):PropMap = {
    (emptyProps /: propsIn) { case (props, (propId, propVal)) =>
      val newId = mappedOID(propId)
      val oldPropOpt = stateOriginal.prop(propId)
      val newVal = oldPropOpt match {
        case Some(prop) if (prop.pType == LinkType) => {
          val rawOIDs = propVal.rawList(LinkType)
          val newElemValues = rawOIDs.map(mappedOID(_)).map(LinkType(_))
          prop.cType.makePropValue(newElemValues, LinkType)
        }
        case Some(prop) if (prop.pType.isInstanceOf[ModelTypeDefiner#ModelType]) => {
          val mtIn = prop.pType.asInstanceOf[ModelTypeDefiner#ModelType]
          val rawVals = propVal.rawList(mtIn)
          val mtOut = state.typ(mappedOID(mtIn.id)).asInstanceOf[ModelTypeDefiner#ModelType]
          val mappedVals = rawVals.map { bundle => 
            mtOut(SimplePropertyBundle(remapPropMap(bundle.props, state, stateOriginal)))
          }
          prop.cType.makePropValue(mappedVals, mtOut)
        }
        case _ => propVal
      }
      props + (newId -> newVal)
    }
  }
  
  private def remapTypes(stateOriginal:SpaceState)(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.types.values) { (curState, typ) =>
      typ match {
        case mt:ModelTypeDefiner#ModelType => {
          val newId = mappedOID(mt.id)
          val newType = ModelType(
            newId,
            mappedOID(curState.id),
            mappedOID(mt.mId),
            mappedOID(mt.basedOn),
            remapPropMap(mt, curState, stateOriginal)
          )
          val newTypes = (curState.types - mt.id) + (newId -> newType) 
          curState.copy(types = newTypes)
        }
        case _ => curState
      }
    }
  }
  
  private def remapProps(stateOriginal:SpaceState)(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.spaceProps.values) { (curState, prop) =>
      val newId = mappedOID(prop.id)
      val newProp = prop.copy(
        i = newId,
        s = mappedOID(curState.id),
        m = mappedOID(prop.model),
        pType = curState.typ(mappedOID(prop.pType.id)).asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]],
        // NOTE: not yet dealing with custom Collections!
        pf = remapPropMap(prop, curState, stateOriginal)
      )
      val newProps = (curState.spaceProps - prop.id) + (newId -> newProp) 
      curState.copy(spaceProps = newProps)
    }
  }
  
  private def remapThings(stateOriginal:SpaceState)(stateIn:SpaceState)(implicit oidMap:Map[OID, OID]):SpaceState = {
    (stateIn /: stateIn.things.values) { (curState, t) =>
      val newId = mappedOID(t.id)
      val newThing = t.copy(
        i = newId,
        s = mappedOID(curState.id), 
        m = mappedOID(t.model), 
        pf = remapPropMap(t, curState, stateOriginal))
      val newThings = (curState.things - t.id) + (newId -> newThing)
      curState.copy(things = newThings)
    }
  }
}
