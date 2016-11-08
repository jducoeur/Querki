package querki.apps

import models.{AsOID, ThingId}

import querki.api.{AutowireParams, OperationHandle, ProgressActor, SpaceApiImpl}
import querki.data.{SpaceInfo, TID}
import querki.globals._
import querki.spaces.Remapper
import querki.spaces.messages._
import querki.values.SpaceVersion

/**
 * @author jducoeur
 */
class AppsFunctionsImpl(info:AutowireParams)(implicit e:Ecology) 
  extends SpaceApiImpl(info, e) with AppsFunctions with ExtracteeComputer with Remapper with Hollower
{  
  import AppsFunctions._
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Core = interface[querki.core.Core]
  lazy val Links = interface[querki.links.Links]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemState:SpaceState = System.State
  lazy val id = state.id
  
  def doRoute(req:Request):Future[String] = route[AppsFunctions](this)(req)
  
  def addApp(appIdStr:String):Future[Unit] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    ThingId(appIdStr) match {
      case AsOID(appId) => {
        // For the time being, we simply assume that you want the current version of the App:
        (spaceRouter ? SpacePluginMsg(user, state.id, AddApp(appId, SpaceVersion(Int.MaxValue)))) map {
          case ThingFound(_, _) => ()
          case ThingError(ex, _) => throw ex
        }
      }
      case _ => throw new PublicException("Apps.notASpace")
    }
  }
  
  /**
   * Extracts an App from this Space, based on the received parameters.
   * 
   * Note that much of the guts of this enormous function is pulled out into separate classes.
   */
  def extractApp(elements:Seq[TID], name:String):Future[Unit] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    // First, take the list of Things to extract, and turn it into the State of the prospective App...
    val extractees = computeExtractees(elements, name, user)(state)
    val appState = extractees.state
    for {
      // ... take extractees.extractState -- the raw version of the App -- and produce a version of it that
      // has all of its OIDs remapped...
      (remappedApp, idMap) <- remapOIDs(appState, extractees.extractState)
      // ... "hollow" out all of the Things that got extracted up to the App, marking them as Shadows.
      hollowedSpace = hollowSpace(extractees, state, appState, idMap)
    }
      yield ()
  }
  
  val stylesheetId = querki.css.MOIDs.StylesheetBaseOID
  
  def getExtractableModels():Future[Seq[ExtractableModelInfo]] = {
    val infoFuts = 
      state.allModels
      .filter { model => 
        if (model.s.id == state.id)
          true
        else if (model.id == stylesheetId)
          // Include Stylesheet only iff there are local Stylesheets defined:
          state.descendants(model.id, false, true, false).size > 0
        else
          false
      }
    .map { model =>
      val canExtract = model.s.id == state.id
      val extractInstancesByDefault = {
        // We want to extract the instances if they are Stylesheets...
        model.id == stylesheetId ||
        // ... or are Choices
        model.ifSet(Links.NoCreateThroughLinkProp)(state)
      }
      
      model.nameOrComputed(rc, state) map { displayName =>
        ExtractableModelInfo(
          ClientApi.thing2TID(model), 
          model.linkName, 
          displayName,
          canExtract,
          extractInstancesByDefault
        )
      }
    }
    
    Future.sequence(infoFuts.toSeq)
  }
}
