package querki.apps

import akka.actor.ActorRef

import org.querki.requester._

import models.{AsOID, Kind, ThingId}
import models.Thing.PropMap

import querki.api.{AutowireParams, OperationHandle, ProgressActor, SpaceApiImpl}
import querki.cluster.OIDAllocator._
import querki.data.{SpaceInfo, TID}
import querki.globals._
import querki.identity.User
import querki.spaces.{PersistentSpaceActor, RealRTCAble, SpaceCreator, StatusNormal}
import querki.spaces.messages._
import querki.values.SpaceVersion

/**
 * @author jducoeur
 */
class AppsFunctionsImpl(info:AutowireParams)(implicit e:Ecology) 
  extends SpaceApiImpl(info, e) with AppsFunctions with AppExtractorSupport[RequestM] with SpaceCreator
{  
  import AppsFunctions._
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Cluster = interface[querki.cluster.QuerkiCluster]
  lazy val Core = interface[querki.core.Core]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Links = interface[querki.links.Links]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemState:SpaceState = System.State
  lazy val id = state.id
  lazy val persister = SpacePersistenceFactory.getSpaceManagerPersister(context)
  
  def doRoute(req:Request):Future[String] = route[AppsFunctions](this)(req)
  
  def addApp(appIdStr:String):Future[Unit] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    ThingId(appIdStr) match {
      case AsOID(appId) => {
        // For the time being, we simply assume that you want the current version of the App:
        (spaceRouter ? SpacePluginMsg(user, state.id, AddApp(appId, SpaceVersion(Int.MaxValue)))) map {
          case AddAppResult(exOpt) => {
            exOpt.map(ex => throw ex)
            ()
          }
        }
      }
      case _ => throw new PublicException("Apps.notASpace")
    }
  }
  
  /* **********************************
   * AppExtractorSupport
   * 
   * These functions provide the Akka glue for extractApp(). They are replaced with dummies for unit testing.
   */
  def getOIDs(nRequested:Int):RequestM[Seq[OID]] = {
    Cluster.oidAllocator.requestFor[NewOIDs](GiveOIDBlock(nRequested)).map(_.oids)
  }
  def createSpace(user:User, spaceId:OID, name:String, display:String):RequestM[OID] = {
    createSpace(user, spaceId, name, display, StatusNormal)
  }
  def setAppState(state:SpaceState):RequestM[SpaceState] = {
    val appId = state.id
    val appRef = context.actorOf(PersistentSpaceActor.actorProps(ecology, SpacePersistenceFactory, requester.self, appId))
    for {
      // ... give it its initial state...
      ThingFound(_, newState) <- appRef.request(SetState(user, appId, state))
      // ... shut it down again...
      _ = context.stop(appRef)      
    }
      yield newState
  }
  def setChildState(state:SpaceState):RequestM[Any] = {
    spaceRouter.request(SetState(user, state.id, state))
  }
  def addAppToGallery(props:PropMap):RequestM[Unit] = {
    SpaceOps.spaceRegion.request(CreateThing(IdentityAccess.SystemUser, MOIDs.GallerySpaceOID, Kind.Thing, MOIDs.GalleryEntryModelOID, props, localCall = false)) map {
      case ThingAck(_) => ()
      case other => throw new Exception(s"appAppToGallery() got unexpected response $other")
    }
  }

  
  /**
   * Extracts an App from this Space, based on the received parameters.
   * 
   * Note that much of the guts of this enormous function is pulled out into separate classes.
   */
  def extractApp(elements:Seq[TID], display:String, summary:String, details:String):Future[SpaceInfo] = {
    val extractor = new AppExtractor(state, user)(RealRTCAble, this)
    extractor.extractApp(elements, display, summary, details).map(ClientApi.spaceInfo(_, user))
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
