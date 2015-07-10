package querki.notifications

import akka.actor._
import akka.event.LoggingReceive

import org.querki.requester._

import models.OID

import querki.api.{AutowireParams, ClientRequest}
import querki.globals._
import Implicits.execContext
import querki.identity.{UserRouteableMessage, UserId}
import querki.notifications.NotificationPersister._
import querki.util.ClusterTimeoutChild
import querki.values.RequestContext

/**
 * Wrapper for the state of this user's notifications.
 * 
 * At the moment, this state blob is shared between UserNotificationActor and its NotificationFunctionsImpls.
 * This is technically legal, since the latter run within the context of the former's receive loop, but it is
 * certainly ugly.
 * 
 * TODO: think about better ways to do this.
 */
private [notifications] class UserNotificationState(val notePersister:ActorRef) {
  // This is kept in most-recent-first order:
  var currentNotes:Seq[Notification] = Seq.empty
    
  var lastNoteChecked:Int = 0
}

class UserNotificationActor(val ecology:Ecology) extends Actor with Stash with Requester with
  EcologyMember with ClusterTimeoutChild
{  
  import UserNotificationActor._
  
  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]

  lazy val userId = OID(self.path.name)
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val shared = new UserNotificationState(PersistenceFactory.getNotificationPersister(userId))
  
  def currentMaxNote = {
    if (shared.currentNotes.isEmpty)
      0
    else
      shared.currentNotes.map(_.id).max    
  }
  def nextNoteId:Int = currentMaxNote + 1
  
  override def preStart() = {
    // This will result in a UserInfo message
    // TODO: this shouldn't be going through the NotificationPersister:
    shared.notePersister ! LoadInfo
    super.preStart()
  }
  
  def receive = LoggingReceive (handleRequestResponse orElse {
    case UserNotificationInfo(id, lastChecked) => {
      shared.lastNoteChecked = lastChecked

      // TODO: This is bad! This is loading all of the notifications before we
      // start doing anything, which can slow down startup times significantly! We need
      // to be able to show the UI, and then send the number of new notifications when
      // we have it loaded:
      shared.notePersister.requestFor[CurrentNotifications](Load) foreach { notes =>
        shared.currentNotes = notes.notes.sortBy(_.id).reverse
          
        // Okay, we're ready to roll:
        context.become(mainReceive)
        unstashAll()
      }
    }

    // Hold everything else off until we've created them all:
    case _ => stash()    
  })
  
  def mkParams(rc:RequestContext) = AutowireParams(rc.requesterOrAnon, Some(shared), rc, this, sender)
  
  def mainReceive:Receive = LoggingReceive (handleRequestResponse orElse {
    
    case NewNotification(_, noteRaw) => {
      // We decide what the actual Notification Id is:
      val note = noteRaw.copy(id = nextNoteId)
      
      shared.notePersister ! NewNotification(userId, note)
      
      shared.currentNotes = note +: shared.currentNotes
    }
    
    case ClientRequest(req, rc) => {
      // Note that, in theory, NotificationFunctions is the only thing that'll be routed here:
      ApiInvocation.handleSessionRequest(req, mkParams(rc))
    }    
  })
}

object UserNotificationActor {
  def actorProps(ecology:Ecology) = Props(classOf[UserNotificationActor], ecology)

  /**
   * Fire-and-forget message, telling this UserSession that they are receiving a new Notification.
   */
  case class NewNotification(userId:UserId, note:Notification) extends UserRouteableMessage {
    def toUser(newUserId:UserId) = copy(userId = newUserId)
  }
  case class RecentNotifications(notes:Seq[Notification])
}
