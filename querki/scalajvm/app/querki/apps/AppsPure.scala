package querki.apps

import models._
import querki.globals._
import querki.spaces.SpaceMessagePersistence._
import querki.time.DateTime
import querki.types.ModelTypeDefiner

/**
 * This is basically a chunk of SpacePure, refactored out for conceptual cleanness. These are
 * the pure stateless functions at the heart of manipulating the Apps for a Space.
 */
trait AppsPure extends ModelPersistence with ModelTypeDefiner with EcologyMember {

  private lazy val Apps = interface[Apps]
  private lazy val Core = interface[querki.core.Core]

  private lazy val NotInheritedProp = Core.NotInheritedProp

  def addFilledAppPure(
    filledApp: SpaceState,
    shadowMapping: Map[OID, OID],
    modTime: DateTime,
    afterExtraction: Boolean
  )(
    state: SpaceState
  ): SpaceState = {
    // First, add the Apps themselves to the Space
    val stateWithApps =
      state.copy(apps = state.apps :+ filledApp, appInfo = state.appInfo :+ (filledApp.id, filledApp.version))

    if (afterExtraction)
      // This App was just extracted from this Space, so we don't need to add any new shadows:
      stateWithApps
    else {
      val shadowRoots: Set[OID] =
        stateWithApps.things.values.filter(_.ifSet(Apps.ShadowFlag)(stateWithApps)).map(
          Apps.getShadowedThing(_)(stateWithApps)
        ).map(_.id).toSet

      // Then, add Shadows for all of the Models in the App
      val stateWithShadows = (stateWithApps /: shadowMapping) { case (curState, (appModelId, shadowId)) =>
        val original = filledApp.thing(appModelId)
        if (original.model == querki.identity.MOIDs.PersonOID)
          // Don't shadow Person records:
          curState
        else if (shadowRoots.contains(Apps.getShadowedThing(original)(curState)))
          // Don't shadow it if we're already shadowing this root:
          curState
        else {
          // Basically, we need to copy in everything that's marked as NotInherited:
          val propsToCopy = original.props.filter { case (propId, propVal) =>
            filledApp.prop(propId).map(_.ifSet(NotInheritedProp)(filledApp)).getOrElse(false)
          } + Apps.ShadowFlag(true)
          val shadow =
            ThingState(
              shadowId,
              state.id,
              appModelId,
              propsToCopy,
              modTime,
              Kind.Thing
            )
          curState.copy(things = curState.things + (shadowId -> shadow))
        }
      }

      // Finally, copy all of the Types, pointing them at their local Shadow Models:
      val stateWithTypes = (stateWithShadows /: filledApp.types.values) { (curState, tpe) =>
        tpe match {
          case mt: ModelTypeDefiner#ModelType => {
            val propsToCopy = mt.props.filter { case (propId, propVal) =>
              filledApp.prop(propId).map(_.ifSet(NotInheritedProp)(filledApp)).getOrElse(false)
            }
            val newType = ModelType(
              mt.id,
              state.id,
              mt.mId,
              shadowMapping(mt.basedOn),
              propsToCopy
            )
            curState.copy(types = curState.types + (mt.id -> newType))
          }
          case _ => curState
        }
      }

      // Then, copy all of the Properties into the Space:
      val stateWithProps = (stateWithTypes /: filledApp.spaceProps.values) { (curState, prop) =>
        // Iff this Property is pointing to a Complex Type, we need to use the local version of that:
        val newType = curState.types.getOrElse(prop.pType.id, prop.pType)
        curState.copy(
          spaceProps = curState.spaceProps +
            (prop.id -> prop.copy(
              s = state.id,
              pType = newType.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
            ))
        )
      }

      stateWithProps
    }
  }

  /**
   * This recursive function takes a "raw" App, straight from deserialization, and adds it to the given state.
   *
   * @param rawApp The state of an App, just deserialized
   * @param addedApps Any other raw Apps that were serialized with it (which should be new parents)
   * @param existingApps Fully-fleshed out Apps that already have their parents
   * @param state The SpaceState that we are adding this App to.
   * @returns The state, with the fleshed-out App added, and the new version of existingApps.
   */
  def addAppPure(
    rawApp: SpaceState,
    addedApps: Map[OID, SpaceState],
    existingApps: Map[OID, SpaceState],
    shadowMapping: Map[OID, OID],
    modTime: DateTime,
    afterExtraction: Boolean
  )(
    state: SpaceState
  ): SpaceState = {
    val (filledApp, allApps) = fillInApps(rawApp, addedApps, existingApps)
    addFilledAppPure(filledApp, shadowMapping, modTime, afterExtraction)(state)
  }

  /**
   * Given a Space in "raw" state -- with its list of appInfo defined but *not* its Apps -- plus the relevant
   * Apps, this fills in the Apps list. This is complicated mainly because it has to be done recursively for
   * parent Apps.
   *
   * We carry existingApps with us so that we only wind up with one copy of any given Space in the tree.
   *
   * @param rawSpace The Space that we're filling in the Apps for.
   * @param addedApps The list of *raw* App states we have available.
   * @param existingApps The *complete* App states we have available.
   * @returns The Space, with the fleshed-out Apps added, and the new version of existingApps.
   */
  def fillInApps(
    rawSpace: SpaceState,
    addedApps: Map[OID, SpaceState],
    existingApps: Map[OID, SpaceState]
  ): (SpaceState, Map[OID, SpaceState]) = {
    // For each *parent* of this App...
    ((rawSpace, existingApps) /: rawSpace.appInfo) { case ((curState, apps), (parentId, parentVersion)) =>
      // ... fill it in...
      val (filledParent, appsNow) = apps.get(parentId) match {
        // We've already built this parent, so we're all set
        case Some(p) => (p, apps)
        // We haven't fleshed out this parent yet:
        case _ => {
          addedApps.get(parentId) match {
            // We have the raw state of this parent, so recurse into it.
            case Some(rawP) => {
              fillInApps(rawP, addedApps, apps)
            }
            // We don't know about this parent at all! Something is horribly wrong!
            case _ => {
              val msg =
                s"State ${rawSpace.displayName}, app ${curState.displayName} asked for unknown Parent App $parentId"
              QLog.error(msg)
              throw new Exception(msg)
            }
          }
        }
      }

      // ... and fill it in.
      (curState.copy(apps = curState.apps :+ filledParent), appsNow)
    }
  }

  def evolveStateApps(stateOpt: Option[SpaceState]): PartialFunction[SpaceEvent, SpaceState] = {

    case DHAddApp(req, modTime, appState, parentApps, shadowMapping, afterExtraction) => {
      implicit val s = stateOpt.get
      val newApps = parentApps.map(rehydrate(_))
      val addedApps = (Map.empty[OID, SpaceState] /: newApps) { (acc, app) =>
        acc + (app.id -> app)
      }
      addAppPure(rehydrate(appState), addedApps, s.allApps, shadowMapping, modTime, afterExtraction.getOrElse(false))(s)
    }

  }
}
