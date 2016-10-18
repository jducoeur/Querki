package querki.apps

import models._
import querki.globals._
import querki.spaces.SpaceMessagePersistence._

/**
 * This is basically a chunk of SpacePure, refactored out for conceptual cleanness. These are
 * the pure stateless functions at the heart of manipulating the Apps for a Space.
 */
trait AppsPure extends ModelPersistence with querki.types.ModelTypeDefiner with EcologyMember {

  def addFilledAppPure(filledApp:SpaceState)(state:SpaceState):SpaceState = {
    state.copy(apps = state.apps :+ filledApp, appInfo = state.appInfo :+ (filledApp.id, filledApp.version))    
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
  def addAppPure(rawApp:SpaceState, addedApps:Map[OID, SpaceState], existingApps:Map[OID, SpaceState])(state:SpaceState):SpaceState = {
    val (filledApp, allApps) = fillInApps(rawApp, addedApps, existingApps)
    addFilledAppPure(filledApp)(state)
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
  def fillInApps(rawSpace:SpaceState, addedApps:Map[OID, SpaceState], existingApps:Map[OID, SpaceState]):(SpaceState, Map[OID, SpaceState]) = {  
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
              val msg = s"State ${rawSpace.displayName}, app ${curState.displayName} asked for unknown Parent App $parentId"
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
  
  def evolveStateApps(stateOpt:Option[SpaceState]):PartialFunction[SpaceEvent, SpaceState] = {
    
    case DHAddApp(req, modTime, appState, parentApps) => {
      implicit val s = stateOpt.get
      val newApps = parentApps.map(rehydrate(_))
      val addedApps = (Map.empty[OID, SpaceState] /: newApps) { (acc, app) =>
        acc + (app.id -> app)
      }
      addAppPure(rehydrate(appState), addedApps, s.allApps)(s)
    }
    
  }
}