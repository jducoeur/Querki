package querki.session

import akka.actor._
import akka.event.LoggingReceive

import upickle._
import autowire._

import org.querki.requester._

import models.OID

import querki.globals._
import Implicits.execContext
import querki.notifications._
import querki.notifications.NotificationPersister._
import messages.{ClientRequest, ClientResponse}
import querki.values.RequestContext

// TODO: this is still too incestuous with UserSession per se.
class UserNotificationActor(userId:OID, val ecology:Ecology, userSession:ActorRef) extends Actor with Stash with Requester with
  autowire.Server[String, upickle.Reader, upickle.Writer] with EcologyMember
{
  import UserSessionMessages._
  
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val SessionInvocation = interface[SessionInvocation]

  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)
  
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.write(r)
  def read[Result: Reader](p: String) = upickle.read[Result](p)
  
  // This is kept in most-recent-first order:
  var currentNotes:Seq[Notification] = Seq.empty
    
  var lastNoteChecked:Int = 0
  
  // How many of the Notifications are new since this User last looked at the Notifications Window?
  def numNewNotes:Int = {
    // TODO: once we have machinery to mark notes as Read, we should filter on that here:
    val newNotes = currentNotes.filter(note => (note.id > lastNoteChecked)/* && !note.isRead*/)
    newNotes.size
  }
  
  def currentMaxNote = {
    if (currentNotes.isEmpty)
      0
    else
      currentNotes.map(_.id).max    
  }
  def nextNoteId:Int = currentMaxNote + 1
  
  override def preStart() = {
    // This will result in a UserInfo message
    // TODO: this shouldn't be going through the NotificationPersister:
    notePersister ! LoadInfo
    super.preStart()
  }
  
  def receive = LoggingReceive (handleRequestResponse orElse {
    case msg @ UserInfo(id, version, lastChecked) => {
      lastNoteChecked = lastChecked

      // TODO: This the overly-coupled bit. Initializing the UserSession and Notifications
      // should be separate, not the same message:
      userSession.forward(msg)

      // TODO: This is broken! This is loading all of the notifications before we
      // start doing anything, which can slow down startup times significantly! We need
      // to be able to show the UI, and then send the number of new notifications when
      // we have it loaded:
      notePersister.requestFor[CurrentNotifications](Load) foreach { notes =>
        currentNotes = notes.notes.sortBy(_.id).reverse
          
        // Okay, we're ready to roll:
        context.become(mainReceive)
        unstashAll()
      }
    }

    // Hold everything else off until we've created them all:
    case msg:UserSessionMsg => stash()    
  })
  
  def mkParams(rc:RequestContext) = AutowireParams(rc.requesterOrAnon, None, rc, None, this, sender)
  
  def mainReceive:Receive = LoggingReceive (handleRequestResponse orElse {
    
    case NewNotification(_, noteRaw) => {
      // We decide what the actual Notification Id is:
      val note = noteRaw.copy(id = nextNoteId)
      
      notePersister ! NewNotification(userId, note)
      
      currentNotes = note +: currentNotes
    }
    
    case FetchSessionInfo(_) => {
      // TODO: make this real. This doesn't conceptually belong here, but until there is more
      // than just the number of notes, we might as well put it here:
      sender ! UserSessionInfo(numNewNotes)
    }
    
    case UserSessionClientRequest(_, ClientRequest(req, rc)) => {
      // Note that, in theory, NotificationFunctions is the only thing that'll be routed here:
      SessionInvocation.handleSessionRequest(req, mkParams(rc))
    }    
  })
}

object UserNotificationActor {
  def actorProps(userId:OID, ecology:Ecology, userSession:ActorRef) = Props(classOf[UserNotificationActor], userId, ecology, userSession)
}
