package querki.publication

import scala.math.Ordering._

import cats._
import cats.implicits._

import akka.actor.Actor.Receive
import akka.persistence.RecoveryCompleted

import org.querki.funcakka._
import org.querki.funcakka.Implicits._

import models._
import querki.globals._
import querki.identity.{IdentityId, PublicIdentity, User}
import querki.time.{DateTime, DateTimeOrdering}
import querki.values.RequestContext

import PublicationCommands._
import PublicationEvents._

trait PublicationCore extends PublicationPure with PersistentActorCore with EcologyMember with querki.types.ModelTypeDefiner with ModelPersistence {
  
  private lazy val Person = interface[querki.identity.Person]
  private lazy val System = interface[querki.system.System]
  
  //////////////////////////////////////////////////
  //
  // Abstract members
  //
  // These are all implemented very differently in the asynchronous, Akka Persistence-based, real Actor
  // vs. the synchronous test implementation.
  //
  
  /**
   * Fetch the Identities for the given IDs.
   */
  def getIdentities(identityIds:Seq[IdentityId]):ME[Map[OID, PublicIdentity]]
  
  /**
   * The OID of this Space.
   */
  def id:OID
  
  //////////////////////////////////////////////////
  
  def toPersistenceId(id:OID) = "publish-" + id.toThingId.toString
  def persistenceId = toPersistenceId(id)
  
  /**
   * During Recovery, we build up all of the PublishEvents.
   * 
   * TODO: eventually, we should get smarter about this, starting from a Snapshot. For now, we're loading everything.
   */
  var loadingEvents = Vector.empty[PublishEvent]
  
  /**
   * True until Recovery is *fully* complete, including post-Recovery, pre-Start activities.
   * 
   * TODO: this might want to get abstracted into PersistentActorCore, since it is often needed.
   */
  var initializing = true
  
  var curState = PublicationState(Vector.empty)
  
  def doPublish(who:User, thingIds:Seq[OID], meta:PropMap, spaceState:SpaceState):ME[PublicationState] = {
    implicit val state = spaceState
    Person.localIdentities(who).headOption match {
      case Some(identity) => {
        val things = thingIds.map(state.anything(_)).flatten
        // Note that we use the *public* context to render the Things, to avoid information leakage:
        val publicRequestContext = RequestContext(None, state.owner)
        // TODO: ah, damn -- rendering is explicitly Future-based, which defeats our attempt to completely
        // hide behind MonadError. Fixing this will require pushing the MonadErrors way down into the stack,
        // which is going to take a while.
        val thingsWithWikitextFuts = things.map { thing =>
          thing.render(publicRequestContext, state).map((thing.id, _))
        }
        val oneFut = fromFuture(Future.sequence(thingsWithWikitextFuts))
        for {
          wikitextPairs <- oneFut
          infos = wikitextPairs.map { case (oid, wikitext) => PublishedThingInfo(oid, wikitext.display.toString, wikitext.strip.toString) }
          publishEvent = PublishEvent(identity.id, infos, meta, DateTime.now)
          persisted <- persistAnd(publishEvent)
          rawEvent = RawPublishEvent(identity, infos.map(info => (info.thingId, info)).toMap, meta, persisted.when)
          _ = curState = addPublication(rawEvent, curState)
        }
          yield curState
      }
      case _ => {
        val ex = new Exception(s"Somehow, User $who tried to Publish in ${state.id}, but doesn't have a local Identity!")
        QLog.error("Error in PublicationCore.doPublish()", ex)
        monadError.raiseError(ex)
      }
    }
  }

  def receiveCommand:Receive = handleRequestResponse orElse {
    case _ if (initializing) => stash()
    
    case Publish(who, things, meta, spaceState) => {
      doPublish(who, things, meta, spaceState)
      // TODO: need to send a message to the Space itself, to change the permissions of the Things.
    }
    
    case Update(who, things, meta, spaceState) => {
      doPublish(who, things, meta, spaceState)
    }
    
    case GetEvents(who, since, until, changesTo, includeMinor, coalesce) => {
      // These give us nice infix comparators for DateTime:
      val dateOrder = implicitly[Ordering[DateTime]]
      import dateOrder._
      
      def includeEvent(evt:RawPublishEvent):Boolean = {
        // Is it in the date range?
        (since.map(evt.when >= _)).getOrElse(true) &&
        (until.map(evt.when <= _)).getOrElse(true) &&
        // Does it have one of the desired Things?
        (
          if (changesTo.isEmpty)
            true
          else {
            changesTo.exists(evt.things.contains(_))
          }
        )
        // TODO: deal with includeMinor
      }
      
      val filtered = curState.events.filter(includeEvent)
      // TODO: deal with coalesce
      sender ! RequestedEvents(filtered)
    }
  }
  
  def receiveRecover:Receive = {
    case evt @ PublishEvent(who, things, meta, when) => {
      // For now, we just build up the list:
      loadingEvents = loadingEvents :+ evt
    }
    
    case RecoveryCompleted => {
      val identityIds = loadingEvents.map(_.who)
      getIdentities(identityIds).map { identities =>
        implicit val system = System.State
        val fullEvents = loadingEvents.map { evt =>
          RawPublishEvent(identities(evt.who), evt.things.map(info => (info.thingId, info)).toMap, evt.meta, evt.when)
        }
        curState = PublicationState(fullEvents)
        initializing = false
        unstashAll()
      }
    }
  }
}
