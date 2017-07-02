package querki.publication

import cats._
import cats.implicits._

import akka.actor.Actor.Receive
import akka.persistence._

import funcakka.PersistentActorCore

import querki.globals._
import querki.persistence._
import querki.spaces.{SpaceMessagePersistenceBase, SpacePure}
import querki.spaces.SpaceMessagePersistence._
import querki.time.DateTime

/**
 * Message published to the troupe when there is new information in the Publication State. It
 * is the responsibility of the UserSpaceSessions to incorporate this.
 * 
 * Note that this is persistable, since it is effectively the snapshot state for the
 * InPublicationStateActor.
 */
case class CurrentPublicationState(
  @KryoTag(1) changes:Map[OID, Vector[SpaceEvent]]
) extends UseKryo

/**
 * This represents the main logic of the InPublicationStateActor. This is a PersistentActor that just stores
 * the Space Events for Publishables, which don't move over to the main Space Actor until those Things
 * get Published. It maintains a secondary "SpaceState", which is just the sum of those Events, so that
 * Users who have Publication access can overlay it onto the public Space.
 */
trait InPublicationStateCore extends SpacePure with PersistentActorCore with SpaceMessagePersistenceBase with EcologyMember {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemState = System.State
  
  /**
   * We're just going to reuse the interval from SpaceState.
   */
  def getSnapshotInterval = Config.getInt("querki.space.snapshotInterval", 100)
  lazy val snapshotInterval = getSnapshotInterval
  
  //////////////////////////////////////////////////
  //
  // Abstract members
  //
  // These are all implemented very differently in the asynchronous, Akka Persistence-based, real Actor
  // vs. the synchronous test implementation.
  //

  /**
   * Tell the rest of the troupe about this.
   */
  def notifyChanges(curState:CurrentPublicationState):Unit
  
  def respondWithState(curState:CurrentPublicationState):Unit
  
  def respondPublished():Unit
  
  /////////////////////////////////////////////////
  
  def toPersistenceId(id:OID) = "inpubstate-" + id.toThingId.toString
  def persistenceId = toPersistenceId(id)
  
  // TODO: this snapshot code is roughly copied from SpaceCore. It should, perhaps, be lifted
  // to PersistentActorCore.
  var snapshotCounter = 0
  def doSaveSnapshot() = {
    saveSnapshot(pState)
    snapshotCounter = 0
  }
  def checkSnapshot() = {
    if (snapshotCounter > snapshotInterval)
      doSaveSnapshot()
    else
      incrementSnapshot()
  }
  def incrementSnapshot() = {
    snapshotCounter += 1
  }
  
  var _pState:Option[CurrentPublicationState] = None
  def pState = _pState.getOrElse(CurrentPublicationState(Map.empty))
  def setState(s:CurrentPublicationState) = {
    _pState = Some(s)
    notifyChanges(s)
  }
  def addEvent(curState:CurrentPublicationState, evt:SpaceEvent):CurrentPublicationState = {
    // TODO: I *really* should be using a lens library here...
    val (isDelete, oid) = evt match {
      case e:DHCreateThing => (false, e.id)
      case e:DHModifyThing => (false, e.id)
      case e:DHDeleteThing => (true, e.id)
      case _ => throw new Exception(s"InPublicationStateCore received unexpected event $evt")
    }
    if (isDelete) {
      curState.copy(changes = curState.changes - oid)
    } else {
      val existing = curState.changes.get(oid).getOrElse(Vector.empty)
      curState.copy(changes = curState.changes + (oid -> (existing :+ evt)))
    }
  }
  
  def receiveCommand:Receive = {
    case evt:SpaceEvent => {
      persistAnd(evt).map { _ =>
        setState(addEvent(pState, evt))
        checkSnapshot()
      }
    }
    
    case AddPublicationEvents(evts) => {
      persistAllAnd(evts).map { _ =>
        val s = (pState /: evts) { (curState, evt) =>
          addEvent(curState, evt)
        }
        setState(s)
        checkSnapshot()
        respondWithState(pState)
      }
    }
    
    // This Thing has been Published, which means we can delete it from our local state:
    case ThingPublished(who, oid) => {
      implicit val s = pState
      val evt = DHDeleteThing(who, oid, DateTime.now)
      persistAnd(evt).map { _ =>
        setState(addEvent(pState, evt))
        checkSnapshot()
        respondPublished()
      }
    }
  }
  
  def receiveRecover:Receive = {
    case SnapshotOffer(metadata, state:CurrentPublicationState) => {
      _pState = Some(state)
    }
    
    case evt:SpaceEvent => {
      // IMPORTANT: we don't call setState(), because we don't want to send out the notifies yet:
      _pState = Some(addEvent(pState, evt))
      incrementSnapshot()
    }
    
    case RecoveryCompleted => {
      // We always fire this, even if it's empty, because UserSpaceSessions relies on it:
      // TODO: yes, this is all horribly incestuous. That's why Publication (and the way we handle SpaceState
      // and requests for it) needs a deep rearchitecting.
      notifyChanges(pState)
    }
  }
}
