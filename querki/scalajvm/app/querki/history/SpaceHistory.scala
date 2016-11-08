package querki.history

import scala.collection.immutable.VectorBuilder

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.persistence._
import akka.persistence.cassandra.query.scaladsl._
import akka.persistence.query._
import akka.stream.ActorMaterializer

import org.querki.requester._

import models._
import Thing.PropMap

import querki.data.TID
import querki.globals._
import querki.history.SpaceHistory._
import querki.identity.User
import querki.identity.IdentityPersistence.UserRef
import querki.spaces.SpaceMessagePersistence._
import querki.time._
import querki.util.{QuerkiActor, SingleRoutingParent, TimeoutChild}
import querki.values.{SpaceState, SpaceVersion}
import querki.spaces.SpacePure
import querki.spaces.messages.CurrentState

import HistoryFunctions._

/**
 * This is a very simplistic wrapper around SpaceHistory, so that the latter can only be in
 * memory when needed. It routes all messages to SpaceHistory.
 * 
 * This lives under SpaceRouter, as part of the standard troupe.
 */
class SpaceHistoryParent(e:Ecology, val id:OID) extends Actor with ReceivePipeline with SingleRoutingParent {
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
  extends QuerkiActor(e) with SpacePure with ModelPersistence with ReceivePipeline with TimeoutChild
{
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Core = interface[querki.core.Core]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val SystemState = interface[querki.system.System].State
  
  def timeoutConfig:String = "querki.history.timeout"
  
  def persistenceId = id.toThingId.toString
  
  lazy val readJournal = PersistenceQuery(context.system).readJournalFor[CassandraReadJournal](CassandraReadJournal.Identifier)
    
  case class HistoryRecord(sequenceNr:Long, evt:SpaceEvent, state:SpaceState)
  type StateHistory = Vector[HistoryRecord]
  val StateHistory = Vector
  
  // The full history of this Space, *one*-indexed. (That seems to be the way Akka Persistence works.)
  // Be careful using this: the 1-indexing isn't at all obvious.
  // TODO: if we start to cope with extracting slices of the history, we'll need to maintain the
  // base index. Note that we can count on sequenceNr increasing monotonically (per discussion in
  // akka-persistence-cassandra Gitter, 9/13/16), so using a Vector makes sense here.
  var history = StateHistory.empty[HistoryRecord]
  
  implicit def OID2TID(oid:OID):TID = ClientApi.OID2TID(oid)
  
  def toSummary(evt:SpaceEvent, sequenceNr:Long, identities:Set[OID], thingNames:ThingNames)(implicit state:SpaceState):(EvtSummary, Set[OID], ThingNames) = 
  {
    def fill(userRef:UserRef, thingIds:Seq[OID], f:String => EvtSummary):(EvtSummary, Set[OID], ThingNames) = {
      val summary = f(userRef.identityIdOpt.map(_.toThingId.toString).getOrElse(""))
      
      val newIdentities = userRef.identityIdOpt match {
        case Some(identityId) => identities + identityId
        case None => identities
      }
      
      val namePairs = for {
        thingId <- thingIds
        thing <- state.anything(thingId)
      }
        yield (thingId, thing.displayName)
        
      val newThingNames = 
        (thingNames /: namePairs) { (tn, pair) =>
          val (id, name) = pair
          tn + (OID2TID(id) -> name)
        }

      (summary, newIdentities, newThingNames)
    }
    
    evt match {
      case DHSetState(dh, modTime) => (BootSummary(sequenceNr, "", modTime.toTimestamp), identities, thingNames)
      
      case DHInitState(userRef, display) => fill(userRef, Seq.empty, ImportSummary(sequenceNr, _, 0))
      
      case DHCreateThing(req, thingId, kind, modelId, dhProps, modTime) => 
        fill(req, dhProps.keys.toSeq :+ thingId, CreateSummary(sequenceNr, _, modTime.toTimestamp, kind, thingId, modelId))
      
      case DHModifyThing(req, thingId, modelIdOpt, propChanges, replaceAllProps, modTime) => 
        fill(req, propChanges.keys.toSeq :+ thingId, ModifySummary(sequenceNr, _, modTime.toTimestamp, thingId, propChanges.keys.toSeq.map(OID2TID)))
      
      case DHDeleteThing(req, thingId, modTime) => 
        fill(req, Seq(thingId), DeleteSummary(sequenceNr, _, modTime.toTimestamp, thingId))
        
      case DHAddApp(req, modTime, app, appParents) =>
        fill(req, Seq(app.id), AddAppSummary(sequenceNr, _, modTime.toTimestamp, app.id))
    }
  }
  
  /**
   * This reads all of the history since the last time it was called. It is designed so that we can
   * reload the client page and get any new events since it was last shown.
   */
  def readCurrentHistory():RequestM[Unit] = {
    // TODO: this is utterly *profligate* with RAM. Think about how we might want to restructure this in
    // order to not hold the entire thing in memory all the time, while still providing reasonably
    // responsive access. Should we instead rebuild stuff on-demand? Should the client signal what
    // range of events it is looking at, and we load those in?
    val source = readJournal.currentEventsByPersistenceId(persistenceId, history.size + 1, Int.MaxValue)
    implicit val mat = ActorMaterializer()
    val initialState =
      if (history.isEmpty)
        emptySpace
      else
        history.last.state
    loopback {
      // Note that we construct the history using VectorBuilder, for efficiency:
      source.runFold((initialState, new VectorBuilder[HistoryRecord])) {
        // Note that this quite intentionally rejects anything that isn't a SpaceEvent!
        case (((curState, history), EventEnvelope(offset, persistenceId, sequenceNr, evt:SpaceEvent))) =>
        val nextState = evolveState(Some(curState))(evt)
        history += HistoryRecord(sequenceNr, evt, nextState)
        (nextState, history)
      }
    } map { fullHist =>
      history = history ++ fullHist._2.result
      mat.shutdown()
    }    
  }
  
  def doReceive = {
    case GetHistorySummary() => {
      readCurrentHistory() map { _ =>
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
    
    case GetHistoryVersion(v) => {
      readCurrentHistory() map { _ =>
        if (history.size < v) 
          throw new Exception(s"Space $id got a request for unknown History Version $v!")
        
        // Adjust for the 1-indexed version numbers
        val idx = (v - 1).toInt
        val record = history(idx)
        sender ! CurrentState(record.state)
      }
    }
  }
}

object SpaceHistory {
  def actorProps(e:Ecology, id:OID) = Props(classOf[SpaceHistoryParent], e, id)
  
  sealed trait HistoryMessage
  /**
   * Fetches the HistorySummary (as described in the public API).
   */
  case class GetHistorySummary() extends HistoryMessage
  
  /**
   * Fetch a specific version of the history of this Space. Returns a CurrentState().
   */
  case class GetHistoryVersion(v:HistoryVersion) extends HistoryMessage
}
