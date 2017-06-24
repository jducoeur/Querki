package querki.publication

import cats._
import cats.implicits._

import akka.actor.Actor.Receive
import akka.persistence.RecoveryCompleted

import funcakka.PersistentActorCore

import querki.globals._
import querki.spaces.SpacePure
import querki.spaces.SpaceMessagePersistence._

/**
 * This represents the main logic of the InPublicationStateActor. This is a PersistentActor that just stores
 * the Space Events for Publishables, which don't move over to the main Space Actor until those Things
 * get Published. It maintains a secondary "SpaceState", which is just the sum of those Events, so that
 * Users who have Publication access can overlay it onto the public Space.
 */
trait InPublicationStateCore extends SpacePure with PersistentActorCore with EcologyMember {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemState = System.State
  
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
  def notifyChanges(curState:SpaceState):Unit
  
  def respondWithState(curState:SpaceState):Unit
  
  /////////////////////////////////////////////////
  
  def toPersistenceId(id:OID) = "inpubstate-" + id.toThingId.toString
  def persistenceId = toPersistenceId(id)
  
  var _pState:Option[SpaceState] = None
  def pState = _pState.getOrElse(emptySpace)
  
  def receiveCommand:Receive = {
    case evt:SpaceEvent => {
      persistAnd(evt).map { _ =>
        _pState = Some(evolveState(Some(pState))(evt))
        notifyChanges(pState)
      }
    }
    
    case AddPublicationEvents(evts) => {
      persistAllAnd(evts).map { _ =>
        val s = (pState /: evts) { (curState, evt) =>
          evolveState(Some(curState))(evt)
        }
        _pState = Some(s)
        notifyChanges(pState)
        respondWithState(pState)
      }
    }
  }
  
  def receiveRecover:Receive = {
    case evt:SpaceEvent => {
      _pState = Some(evolveState(Some(pState))(evt))
    }
    
    case RecoveryCompleted => {
      // Only send the message if there have been some events. In most Spaces, this won't do anything:
      _pState.foreach { state =>
        notifyChanges(pState)
      }
    }
  }
}
