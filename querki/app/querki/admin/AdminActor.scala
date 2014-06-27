package querki.admin

import scala.concurrent.duration._

import akka.actor._

import models.Wikitext

import querki.ecology._
import querki.identity.User
import querki.notifications.{AllUsers}
import querki.spaces.messages.{GetSpacesStatus, SpaceStatus}

private[admin] class AdminActor(val ecology:Ecology) extends Actor with EcologyMember {
  
  import AdminActor._
  
  lazy val AdminInternal = interface[AdminInternal]
  lazy val Notifications = interface[querki.notifications.Notifications]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  var statusRequestFrom:ActorRef = null
  var statuses:Seq[SpaceStatus] = Seq.empty

  def receive = {
    // TODO: this code is pretty smelly for a long-lived Actor. We probably ought to instead spawn off a worker Actor that does
    // all of this.
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
    
    case SendSystemMessage(req, header, body) => {
      val note = AdminInternal.createMsg(req.mainIdentity, header, body)
      Notifications.send(req, AllUsers, note)
    }
  }
  
}

private[admin] object AdminActor {
  case object StatusTimeout
  
  case class SendSystemMessage(req:User, header:String, body:String)
}