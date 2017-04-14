package querki.publication

import akka.actor.Actor.Receive

import org.querki.funcakka._

import querki.globals._

import PublicationCommands._
import PublicationEvents._

trait PublicationCore extends PublicationPure with PersistentActorCore {
  //////////////////////////////////////////////////
  //
  // Abstract members
  //
  // These are all implemented very differently in the asynchronous, Akka Persistence-based, real Actor
  // vs. the synchronous test implementation.
  //
  
  /**
   * The OID of this Space.
   */
  def id:OID
  
  //////////////////////////////////////////////////
  
  def toPersistenceId(id:OID) = "publish-" + id.toThingId.toString
  def persistenceId = toPersistenceId(id)

  def receiveCommand:Receive = {
    case Publish(who, things, meta, state) => {
      ???
    }
    
    case Update(who, things, meta, state) => {
      ???
    }
    
    case GetEvents(who, since, until, changesTo, includeMinor, viewAsPublic, coalesce) => {
      ???
    }
  }
  
  def receiveRecover:Receive = {
    case PublishEvent(who, things, meta) => {
      ???
    }
  }
}
