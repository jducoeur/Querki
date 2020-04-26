package querki.publication

import scala.math.Ordering._
import cats._
import cats.implicits._
import akka.actor.Actor.Receive
import akka.persistence.RecoveryCompleted
import funcakka._
import funcakka.Implicits._
import models._
import querki.core.QLText
import querki.globals._
import querki.identity.{IdentityId, User, PublicIdentity}
import querki.time.{DateTime, DateTimeOrdering}
import querki.values.RequestContext
import PublicationCommands._
import PublicationEvents._
import querki.spaces.TracingSpace

trait PublicationCore extends PublicationPure with PersistentActorCore with EcologyMember with querki.types.ModelTypeDefiner with ModelPersistence {
  
  private lazy val AccessControl = interface[querki.security.AccessControl]
  private lazy val AWS = interface[querki.aws.AWS]
  private lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  private lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  private lazy val Person = interface[querki.identity.Person]
  private lazy val Publication = interface[Publication]
  private lazy val System = interface[querki.system.System]
  
  //////////////////////////////////////////////////
  //
  // Abstract members
  //
  // These are all implemented very differently in the asynchronous, Akka Persistence-based, real Actor
  // vs. the synchronous test implementation.
  //
  
  /**
   * Change the properties of the specified Things.
   */
  def sendChangeProps(who:User, pairs:Seq[(OID, PropMap)])(implicit state:SpaceState):ME[SpaceState]
  
  /**
   * The OID of this Space.
   */
  def id:OID
  
  //////////////////////////////////////////////////

  lazy val tracing = TracingSpace(id, "PublicationCore: ")
  
  /**
   * Logging wrapper.
   * 
   * This is designed to wrap around something that returns an ME, and make sure that both synchronous and
   * asynchronous exceptions get logged.
   * 
   * TODO: in principle, this should get lifted to the same level as ME itself.
   */
  def loggingErrors[R](f: => ME[R]):ME[R] = {
    val resultME = try {
      f
    } catch {
      case th:Throwable => QLog.logAndThrowException(th)
    }
    resultME.handleError(th => QLog.logAndThrowException(th))
  }
  
  def log(msg: => String):Unit = {
    tracing.trace(msg)
  }
  
  def toPersistenceId(id:OID) = "publish-" + id.toThingId.toString
  def persistenceId = toPersistenceId(id)
  
  /**
   * During Recovery, we build up all of the PublishEvents.
   * 
   * TODO: eventually, we should get smarter about this, starting from a Snapshot. For now, we're loading everything.
   */
  var loadingEvents = Vector.empty[PublishEvent]
  var loadingPublishRSS:Boolean = false
  
  /**
   * True until Recovery is *fully* complete, including post-Recovery, pre-Start activities.
   * 
   * TODO: this might want to get abstracted into PersistentActorCore, since it is often needed.
   */
  var initializing = true
  
  var curState = PublicationState(Vector.empty, false)
  
  case class ThingInfo(thingId:OID, wikitext:Wikitext, displayName:String, notes:Option[QLText])
  
  def doPublish(who:User, thingIds:Seq[OID], meta:PropMap, spaceState:SpaceState):ME[PublicationState] = {
    implicit val state = spaceState
    log(s"doPublish")
    Person.localIdentities(who).headOption match {
      case Some(identity) => {
        log("We have an identity")
        val things = thingIds.map(state.anything(_)).flatten
        log(s"Operating on Things: ${things.map(_.displayName)}")
        // Note that we use the *public* context to render the Things, to avoid information leakage:
        val publicRequestContext = RequestContext(None, state.owner)
        // TODO: ah, damn -- rendering is explicitly Future-based, which defeats our attempt to completely
        // hide behind MonadError. Fixing this will require pushing the MonadErrors way down into the stack,
        // which is going to take a while.
        // Note that MonadError does *not* appear to contain sequence(). Will need to think about that one.
        val thingsWithWikitextFuts = things.map { thing =>
          val notes = thing.getFirstOpt(Publication.PublishNotesProp)
          thing.render(publicRequestContext, state).map(ThingInfo(thing.id, _, thing.displayName, notes))
        }
        val oneFut = fromFuture(Future.sequence(thingsWithWikitextFuts))
        for {
          wikitextPairs <- oneFut
          _ = log(s"wikitextPairs: $wikitextPairs")
          infos = wikitextPairs.map { case ThingInfo(oid, wikitext, displayName, notes) => 
            PublishedThingInfo(oid, wikitext.display.toString, wikitext.strip.toString, displayName)
          }
          notes = ("" /: wikitextPairs) { (text, thingInfo) =>
            thingInfo.notes match {
              case Some(n) => text + n.text
              case None => text
            }
          }
          fullMeta =
            if (notes.isEmpty)
              meta
            else
              meta + Publication.PublishNotesProp(notes)
          publishEvent = PublishEvent(identity.id, infos, fullMeta, DateTime.now)
          _ = log(s"About to persist $publishEvent")
          persisted <- persistAnd(publishEvent)
          rawEvent = RawPublishEvent(identity, infos.map(info => (info.thingId, info)).toMap, fullMeta, persisted.when)
          _ = curState = addPublication(rawEvent, curState)
          _ = log(s"Added to Publication")
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
  
  def publish(who:User, thingIds:Seq[OID], meta:PropMap, spaceState:SpaceState):ME[PublicationState] = {
    if (AccessControl.hasPermission(Publication.CanPublishPermission, spaceState, who, spaceState)) {
      doPublish(who, thingIds, meta, spaceState)
    } else {
      // TODO: in theory, this shouldn't happen, but it should still become a proper PublicException:
      monadError.raiseError(new Exception(s"You don't have permission to Publish!"))
    }
  }
  
  def computePublishChanges(thingIds:Seq[OID])(implicit state:SpaceState):ME[Seq[(OID, PropMap)]] = {
    tracing.trace(s"computePublishChanges")
    (monadError.pure(Seq.empty[(OID, PropMap)]) /: thingIds) { (me, thingId) =>
      state.anything(thingId) match {
        case Some(thing) => {
          val newProps = 
            toProps(
              Publication.PublishedProp(true),
              Publication.HasUnpublishedChanges(false),
              // Clear the Notes:
              Publication.PublishNotesProp(DataModel.getDeletedValue(Publication.PublishNotesProp)))
          me.map(_ :+ (thingId, newProps))
        }
        case _ => monadError.raiseError(new Exception(s"Trying to Publish unknown Thing $thingId!"))
      }      
    }
  }
  def updatePublishedThings(who:User, thingIds:Seq[OID])(implicit state:SpaceState):ME[SpaceState] = {
    log(s"About to computePublishChanges for $thingIds")
    for {
      updates <- computePublishChanges(thingIds)
      _ = log(s"About to sendChangeProps($updates)")
      result <- sendChangeProps(who, updates)
    }
      yield result
  }

  def receiveCommand:Receive = handleRequestResponse orElse {
    case _ if (initializing) => stash()
    
    case Publish(who, things, meta, spaceState) => {
      loggingErrors {
        for {
          _ <- publish(who, things, meta, spaceState)
          finalState <- updatePublishedThings(who, things)(spaceState)
        }
          yield { sender ! PublishResponse(finalState) }
      }
    }
    
    case Update(who, things, meta, spaceState) => {
      loggingErrors {
        for {
          _ <- publish(who, things, meta, spaceState)
          finalState <- updatePublishedThings(who, things)(spaceState)
        }
          yield { sender ! PublishResponse(spaceState) }
      }
    }
    
    case GetEvents(who, since, until, includeMinor, coalesce) => {
      // These give us nice infix comparators for DateTime:
      val dateOrder = implicitly[Ordering[DateTime]]
      import dateOrder._
      
      def includeEvent(evt:OnePublishEvent):Boolean = {
        // Is it in the date range?
        (since.map(evt.when >= _)).getOrElse(true) &&
        (until.map(evt.when <= _)).getOrElse(true) &&
        (
          if (includeMinor)
            // We're including it even if it's a minor update
            true
          else {
            // If it's a minor update, don't include it:
            !evt.isMinor
          }
        )
      }
      
      val filtered = curState.publicEvents.filter(includeEvent)
      // TODO: deal with coalesce
      sender ! RequestedEvents(filtered)
    }
    
    case GetRSSUrl(who) => {
      // TODO: when we get back to RSS
    }
  }
  
  def receiveRecover:Receive = {
    case evt @ PublishEvent(who, things, meta, when) => {
      // For now, we just build up the list:
      loadingEvents = loadingEvents :+ evt
    }
    
    case StartPublishingRSS(who, url, when) => {
      loadingPublishRSS = true
    }
    
    case RecoveryCompleted => {
      val identityIds = loadingEvents.map(_.who)
      fromFuture(IdentityAccess.getIdentities(identityIds.toSeq)).map { identities =>
        implicit val system = System.State
        val fullEvents = loadingEvents.map { evt =>
          RawPublishEvent(identities(evt.who), evt.things.map(info => (info.thingId, info)).toMap, evt.meta, evt.when)
        }
        curState = PublicationState(fullEvents, loadingPublishRSS)
        initializing = false
        unstashAll()
      }
    }
  }
}
