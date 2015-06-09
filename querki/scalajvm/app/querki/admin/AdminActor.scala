package querki.admin

import scala.concurrent.duration._

import akka.actor._

import models.Wikitext

import querki.ecology._
import querki.identity.User
import querki.notifications.{AllUsers}
import querki.spaces.messages.{GetSpacesStatus, SpaceStatus}

/**
 * Supervisor for Admin workers.
 * 
 * IMPORTANT: for the moment, this is a Duplicated Singleton -- that is, there is one distinct instance of this
 * Actor per Node. It mainly is just the parent for workers that do the actual work, so there's no motivation to
 * get fancy about it.
 * 
 * If we ever get anything truly long-running (eg, an open AdminConsole), that probably should be run under the
 * aegis of the UserSession, not under this Actor.
 */
private[admin] class AdminActor(val ecology:Ecology) extends Actor with EcologyMember {
  
  import AdminActor._
  
  def receive = {
    case msg:GetSpacesStatus => {
      // Fire it off to a worker:
      val worker = context.actorOf(Props(classOf[AdminStatusWorker], ecology))
      worker.forward(msg)
    }
    
    case msg:SendSystemMessage => {
      // Fire it off to a worker:
      val worker = context.actorOf(Props(classOf[AdminSystemMessageWorker], ecology))
      worker.forward(msg)
    }
  }
}

/**
 * Worker to deal with a request for a status update. This needs rewriting, but is at least being extracted now
 * to the worker it belongs in.
 */
private [admin] class AdminStatusWorker(val ecology:Ecology) extends Actor with EcologyMember {

  import AdminActor._
  
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  var statusRequestFrom:ActorRef = null
  var statuses:Seq[SpaceStatus] = Seq.empty

  def receive = {
    // TODO: this needs to be rewritten, probably using DistributedPubSub?
    case req @ GetSpacesStatus(requester) => {
      statusRequestFrom = sender
      statuses = Seq.empty
      import context.dispatcher
      context.system.scheduler.scheduleOnce(3 seconds, self, StatusTimeout)
      SpaceOps.spaceManager ! req 
    }
    
    case status:SpaceStatus => statuses = statuses :+ status
    
    case StatusTimeout => {
      // Everyone's had enough time to respond. Wrap it all up...
      statusRequestFrom ! SystemStatus(statuses)
    }    
  }
}

/**
 * Actor to send a SystemMessage. This gets its own worker simply because it is potentially quite
 * long-running. Indeed, this is almost certainly broken, and needs rethinking -- it's not going to
 * scale in the medium term.
 */
private[admin] class AdminSystemMessageWorker(val ecology:Ecology) extends Actor with EcologyMember {
  import AdminActor._
  
  lazy val AdminInternal = interface[AdminInternal]
  lazy val Notifications = interface[querki.notifications.Notifications]
  
  def receive = {
    case SendSystemMessage(req, header, body) => {
      val note = AdminInternal.createMsg(req.mainIdentity, header, body)
      Notifications.send(req, AllUsers, note)
      context.stop(self)
    }    
  }
}

private[admin] object AdminActor {
  case object StatusTimeout
  
  case class SendSystemMessage(req:User, header:String, body:String)
}