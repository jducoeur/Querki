package querki.history

import scala.collection.immutable.VectorBuilder

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.persistence._
import akka.persistence.cassandra.query.scaladsl._
import akka.persistence.query._
import akka.stream.ActorMaterializer

import models._

import querki.data.TID
import querki.globals._
import querki.identity.User
import querki.identity.IdentityPersistence.UserRef
import querki.spaces.SpaceMessagePersistence._
import querki.time._
import querki.util._
import querki.values.{SpaceState, SpaceVersion}
import querki.spaces.SpacePure

import HistoryFunctions._

/**
 * This is a very simplistic wrapper around SpaceHistory, so that the latter can only be in
 * memory when needed. It routes all messages to SpaceHistory.
 */
class SpaceHistoryParent(e:Ecology, val id:OID) extends Actor with SingleRoutingParent with ReceivePipeline {
  def createChild():ActorRef = context.actorOf(Props(classOf[SpaceHistory], e, id))
  
  def receive = {
    case msg => routeToChild(msg)
  }
}

/**
 * This is essentially a variant of the PersistentSpaceActor, which reads in the complete history
 * of a Space, and provides access to it.
 */
private [history] class SpaceHistory(e:Ecology, val id:OID) 
  extends QuerkiBootableActor(e) with SpacePure with ModelPersistence with TimeoutChild with ReceivePipeline
{
  import SpaceHistory._
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Core = interface[querki.core.Core]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val SystemState = interface[querki.system.System].State
  
  def timeoutConfig:String = "querki.history.timeout"
  
  def persistenceId = id.toThingId.toString
  
  lazy val readJournal = PersistenceQuery(context.system).readJournalFor[CassandraReadJournal](CassandraReadJournal.Identifier)
  
  lazy val emptySpace =
    SpaceState(
      id,
      SystemState.id,
      Thing.emptyProps,
      UnknownOID,
      "",
      DateTime.now,
      Seq.empty,
      Some(SystemState),
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      None,
      SpaceVersion(0)
    )
    
  case class HistoryRecord(sequenceNr:Long, evt:Any, state:SpaceState)
  type StateHistory = Vector[HistoryRecord]
  val StateHistory = Vector
  
  // The full history of this Space, zero-indexed.
  // TODO: if we start to cope with extracting slices of the history, we'll need to maintain the
  // base index. Note that we can count on sequenceNr increasing monotonically (per discussion in
  // akka-persistence-cassandra Gitter, 9/13/16), so using a Vector makes sense here.
  var history = StateHistory.empty[HistoryRecord]
  
  override def preStart() = {
    self ! Start
  }
  
  implicit def OID2TID(oid:OID):TID = ClientApi.OID2TID(oid)
  
  def toSummary(evt:Any, sequenceNr:Long, identities:Set[OID], thingNames:ThingNames)(implicit state:SpaceState):(EvtSummary, Set[OID], ThingNames) = 
  {
    def fill(userRef:UserRef, thingIdOpt:Option[OID], f:String => EvtSummary):(EvtSummary, Set[OID], ThingNames) = {
      val summary = f(userRef.identityIdOpt.map(_.toThingId.toString).getOrElse(""))
      
      val newIdentities = userRef.identityIdOpt match {
        case Some(identityId) => identities + identityId
        case None => identities
      }
      
      val thingNameOpt = for {
        thingId <- thingIdOpt
        thing <- state.anything(thingId)
      }
        yield thing.displayName
        
      val newThingNames = thingNameOpt.map(name => thingNames + (OID2TID(thingIdOpt.get) -> name)).getOrElse(thingNames)
      (summary, newIdentities, newThingNames)
    }
    
    evt match {
      case BootSpace(dh, modTime) => (BootSummary(sequenceNr, "", modTime.toTimestamp), identities, thingNames)
      
      case DHInitState(userRef, display) => fill(userRef, None, ImportSummary(sequenceNr, _, 0))
      
      case DHCreateThing(req, thingId, kind, modelId, dhProps, modTime) => 
        fill(req, Some(thingId), CreateSummary(sequenceNr, _, modTime.toTimestamp, kind, thingId, modelId))
      
      case DHModifyThing(req, thingId, modelIdOpt, propChanges, replaceAllProps, modTime) => 
        fill(req, Some(thingId), ModifySummary(sequenceNr, _, modTime.toTimestamp, thingId, propChanges.keys.toSeq.map(OID2TID)))
      
      case DHDeleteThing(req, thingId, modTime) => 
        fill(req, Some(thingId), DeleteSummary(sequenceNr, _, modTime.toTimestamp, thingId))
    }
  }
  
  // Actually process a single event. Note that this intentionally echoes SpaceCore.receiveRecover, but
  // I'm not sure how to factor them together.
  def evolveState(evt:Any, state:SpaceState):SpaceState = {
    evt match {
      case BootSpace(dh, modTime) => rehydrate(dh)
      
      case DHInitState(userRef, display) => initStatePure(userRef.userId, userRef.identityIdOpt.get, None, display)
      
      case DHCreateThing(req, thingId, kind, modelId, dhProps, modTime) => {
        implicit val s = state
        createPure(kind, thingId, modelId, dhProps, modTime)(state)
      }
      
      case DHModifyThing(req, thingId, modelIdOpt, propChanges, replaceAllProps, modTime) => {
        implicit val s = state
        state.anything(thingId).map { thing =>
          modifyPure(thingId, thing, modelIdOpt, propChanges, replaceAllProps, modTime)(state)
        }.getOrElse(state)
      }
      
      case DHDeleteThing(req, thingId, modTime) => {
        state.anything(thingId).map { thing =>
          deletePure(thingId, thing)(state)
        }.getOrElse(state)
      }
    }
  }
  
  def bootReceive = {
    case Start => {
      // When this Actor starts, we run through all events so far, building up an indexed StateHistory
      // TODO: this is utterly *profligate* with RAM. Think about how we might want to restructure this in
      // order to not hold the entire thing in memory all the time, while still providing reasonably
      // responsive access. Should we instead rebuild stuff on-demand? Should the client signal what
      // range of events it is looking at, and we load those in?
      val source = readJournal.currentEventsByPersistenceId(persistenceId, 0, Int.MaxValue)
      implicit val mat = ActorMaterializer()
      loopback {
        // Note that we construct the history using VectorBuilder, for efficiency:
        source.runFold((emptySpace, new VectorBuilder[HistoryRecord])) {
          case (((curState, history), EventEnvelope(offset, persistenceId, sequenceNr, evt))) =>
          val nextState = evolveState(evt, curState)
          history += HistoryRecord(sequenceNr, evt, nextState)
          (nextState, history)
        }
      } foreach { fullHist =>
        history = fullHist._2.result
        mat.shutdown()
        doneBooting()
      }
    }
  }
  
  def doReceive = {
    case GetHistorySummary() => {
      val (evtsBuilder, identityIds, thingNames) = ((new VectorBuilder[EvtSummary], Set.empty[OID], Map.empty[TID, String]) /: history) { (current, historyRec) =>
        val (builder, identities, thingNames) = current
        val (summary, newIdentities, newThingNames) = toSummary(historyRec.evt, historyRec.sequenceNr, identities, thingNames)(historyRec.state)
        (builder += summary, newIdentities, newThingNames)
      }
      
      loopback(IdentityAccess.getIdentities(identityIds.toSeq)) foreach { identities =>
        val identityMap = identities.map { case (id, publicIdentity) =>
          (id.toThingId.toString -> ClientApi.identityInfo(publicIdentity))
        }
        
        sender ! HistorySummary(evtsBuilder.result, EvtContext(identityMap, thingNames))
      }
    }
  }
}

object SpaceHistory {
  def actorProps(e:Ecology, id:OID) = Props(classOf[SpaceHistoryParent], e, id)
  
  private case object Start
  
  sealed trait HistoryMessage
  /**
   * Fetches the HistorySummary (as described in the public API).
   */
  case class GetHistorySummary() extends HistoryMessage
}
