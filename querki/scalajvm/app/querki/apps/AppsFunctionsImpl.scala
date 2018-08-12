package querki.apps

import akka.actor.ActorRef
import akka.persistence._
import akka.persistence.cassandra.query.scaladsl._
import akka.persistence.query._
import akka.stream.ActorMaterializer

import org.querki.requester._

import models._

import querki.api.{AutowireParams, OperationHandle, ProgressActor, SpaceApiImpl}
import querki.cluster.OIDAllocator._
import querki.data.{SpaceInfo, TID, TOID}
import querki.globals._
import querki.history.HistoryFunctions.SetStateReason
import querki.identity.User
import querki.spaces.{PersistentSpaceActor, RealRTCAble, SpaceCreator, StatusNormal}
import querki.spaces.SpaceMessagePersistence._
import querki.spaces.messages._
import querki.time.DateTime
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
  
  /**
   * Figures out what the current version of this App *really* is. That might be later than what
   * the Space shows.
   * 
   * TODO: this is *incredibly* primitive at this point -- it just skims the event log and counts the events.
   * We actually only care about the *last* event; can we get that?
   */
  private def getCurrentAppVersion(appId:OID):Future[(SpaceVersion, DateTime)] = {
    lazy val readJournal = PersistenceQuery(context.system).readJournalFor[CassandraReadJournal](CassandraReadJournal.Identifier)
    val source = readJournal.currentEventsByPersistenceId(appId.toThingId.toString, 0, Int.MaxValue)
    implicit val mat = ActorMaterializer()

    source.runFold((0L, querki.time.epoch)) { 
      case ((n, t), EventEnvelope(offset, persistenceId, sequenceNr, evt)) => 
        evt match {
          case se:SpaceEvent => {
            (sequenceNr, se.modTime)
          }
          case _ => (sequenceNr, t) 
        }
    }
      .map { case (n, t) =>
        mat.shutdown()
        (SpaceVersion(n), t)
      }
  }
  
  def checkAppVersions():Future[Map[TID, AppInfo]] = {
    (Future.successful(Map.empty[TID, AppInfo]) /: state.apps) { (fut, app) =>
      for {
        m <- fut
        (currentVersionNum, currentTime) <- getCurrentAppVersion(app.id)
      }
        yield {
          if (currentVersionNum.v > app.version.v) {
            val tid:TID = app.id
            m + (tid -> AppInfo(tid, AppState(app.version.v, app.modTime.getMillis), AppState(currentVersionNum.v, currentTime.getMillis)))
          } else
            m
        }
    }
  }
  
  def addApp(appIdStr:String):Future[Unit] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    ThingId(appIdStr) match {
      case AsOID(appId) => {
        for {
          appVersion <- getCurrentAppVersion(appId)
          AddAppResult(exOpt, _) <- spaceRouter.requestFor[AddAppResult](SpacePluginMsg(user, state.id, AddApp(appId, appVersion._1, false, false)))
          _ = exOpt.map(ex => throw ex)
        }
          yield ()
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
    val appRef = context.actorOf(PersistentSpaceActor.actorProps(ecology, SpacePersistenceFactory, requester.self, appId, false))
    for {
      // ... give it its initial state...
      ThingFound(_, newState) <- appRef.request(SetState(user, appId, state, SetStateReason.InitialAppState, state.displayName))
      // ... shut it down again...
      _ = context.stop(appRef)      
    }
      yield newState
  }
  def sendSpaceMessage(msg:SpaceMessage):RequestM[OID] = {
    SpaceOps.spaceRegion.request(msg) map {
      case ThingAck(id) => id
      case other => throw new Exception(s"sendSpaceMessage() got unexpected response $other")
    }
  }
  def sendMessageToSelf(msg:SpaceMessage):RequestM[Any] = {
    spaceRouter.request(msg)
  }
  
  /**
   * Extracts an App from this Space, based on the received parameters.
   * 
   * Note that much of the guts of this enormous function is pulled out into separate classes.
   */
  def extractApp(elements:Seq[TID], display:String, summary:String, details:String):Future[SpaceInfo] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
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
