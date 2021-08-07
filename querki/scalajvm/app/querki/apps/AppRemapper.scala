package querki.apps

import models._
import querki.cluster.OIDAllocator._
import querki.globals._
import querki.spaces.RTCAble
import querki.types.{ModelTypeBase, ModelTypeDefiner, SimplePropertyBundle}

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

  implicit def rtc: RTCAble[RM]
  private implicit def rm2rtc[A](rm: RM[A]) = rtc.toRTC(rm)

  def extractorSupport: AppExtractorSupport[RM]

  /**
   * Given a proto-App, this returns that State with all of the OIDs replaced by new ones.
   * Note that this already only contains the Things that are being extracted to the App.
   */
  def remapOIDs(
    state: SpaceState,
    includeSpace: Boolean
  ): RM[(SpaceState, Map[OID, OID])] = {
    val allThings = state.everythingLocal.toSeq

    // First, fetch new OIDs for the Models:
    val (models, instances) = state.things.values.partition(_.isModel(state))
    val (pages, plainInstances) = instances.partition(_.model == querki.basic.MOIDs.SimpleThingOID)
    val oidsToMap = (models.toSeq ++ pages :+ state).map(_.id)
    extractorSupport.getOIDs(oidsToMap.size).map { oids =>
      val pairs = oidsToMap.zip(oids)
      // This is now a Map from old to new OIDs:
      implicit val oidMap = Map(pairs: _*)
//      QLog.spew(s"Remappings:")
//      oidMap.foreach { case (k, v) => QLog.spew(s"  $k -> $v") }
      // IMPORTANT NOTE: the way this works, we're implicitly assuming that Types and Props don't
      // themselves have meta-Props whose *values* are pointing to other Things in this Space. That
      // may or may not be a safe assumption -- keep an eye on it! (The issue is that, until the
      // Props have been remapped, we're going to tend to fail to fetch Prop values.)

      // Note that "and" is magic syntax sugar on SpaceState that lets me chain without creating lots
      // of intermediate names:
      val transformed =
        remapTypes(state).and(
          remapProps
        ).and(
          remapThings
        ).and {
          _.copy(s = oidMap(state.id))
        }.and {
          withOwnID =>
            if (includeSpace)
              withOwnID.copy(pf = remapPropMap(withOwnID, withOwnID))
            else
              withOwnID
        }
      (transformed, oidMap)
    }
  }

  private def mappedOID(oid: OID)(implicit oidMap: Map[OID, OID]): OID = {
    oidMap.get(oid).getOrElse(oid)
  }

  /**
   * Remap any Links in this PropMap, if appropriate.
   */
  private def remapPropMap(
    t: Thing,
    state: SpaceState
  )(implicit
    oidMap: Map[OID, OID]
  ): PropMap = {
    remapPropMap(t.props, state)
  }

  private def remapPropMap(
    propsIn: PropMap,
    state: SpaceState
  )(implicit
    oidMap: Map[OID, OID]
  ): PropMap = {
    (propsIn /: propsIn) { case (props, (propId, propVal)) =>
      val propOpt = state.prop(propId)
      propOpt match {
        case Some(prop) if (prop.pType == LinkType) => {
          val rawOIDs = propVal.rawList(LinkType)
          val newElemValues = rawOIDs.map(mappedOID(_)).map(LinkType(_))
          val newVal = prop.cType.makePropValue(newElemValues, LinkType)
          props.updated(propId, newVal)
        }
        case Some(prop) if (prop.pType.isInstanceOf[ModelTypeDefiner#ModelType]) => {
          val mtIn = prop.pType.asInstanceOf[ModelTypeDefiner#ModelType]
          val rawVals = propVal.rawList(mtIn)
          val mtOut = state.typ(mappedOID(mtIn.id)).asInstanceOf[ModelTypeDefiner#ModelType]
          val mappedVals = rawVals.map { bundle =>
            mtOut(SimplePropertyBundle(remapPropMap(bundle.props, state)))
          }
          val newVal = prop.cType.makePropValue(mappedVals, mtOut)
          props.updated(propId, newVal)
        }
        case _ => props
      }
    }
  }

  private def remapTypes(stateIn: SpaceState)(implicit oidMap: Map[OID, OID]): SpaceState = {
    (stateIn /: stateIn.types.values) { (curState, typ) =>
      typ match {
        case mt: ModelTypeDefiner#ModelType => {
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

  private def remapProps(stateIn: SpaceState)(implicit oidMap: Map[OID, OID]): SpaceState = {
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

  private def remapThings(stateIn: SpaceState)(implicit oidMap: Map[OID, OID]): SpaceState = {
    (stateIn /: stateIn.things.values) { (curState, t) =>
      val newId = mappedOID(t.id)
      val newThing = t.copy(
        i = newId,
        s = mappedOID(curState.id),
        m = mappedOID(t.model),
        pf = remapPropMap(t, curState)
      )
      // Note that we've got a new OID, so we can't just use curState.things.updated():
      val newThings = (curState.things - t.id) + (newId -> newThing)
      curState.copy(things = newThings)
    }
  }
}
