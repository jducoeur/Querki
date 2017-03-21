package querki.admin

import scala.concurrent.duration._

import akka.actor._
import akka.cluster.ddata._
import Replicator._

import models._

import querki.ecology._
import querki.globals._
import querki.identity.{User, UserId}
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
  
  lazy val AdminInternal = interface[AdminInternal]
  
  lazy val TimedSpaceKey = AdminInternal.TimedSpaceKey
  
  import AdminActor._
  
  // We need to hear from the Replicator when a TimedSpace is added or removed, so we
  // can make sure our node is paying attention:
  val replicator = DistributedData(context.system).replicator
  replicator ! Subscribe(TimedSpaceKey, self)
  
  def toWorker[T <: Actor](msg:Any)(implicit tag:scala.reflect.ClassTag[T]) = {
    // Fire it off to a worker:
    val worker = context.actorOf(Props(tag.runtimeClass, ecology))
    worker.forward(msg)    
  }
  
  def receive = {
    case msg:GetSpacesStatus => toWorker[AdminStatusWorker](msg)
    
    case msg:SendSystemMessage => toWorker[AdminSystemMessageWorker](msg)
    
    case msg:GetAllUserIdsForAdmin => toWorker[AdminUserIdFetcher](msg)
    
    case c @ Changed(TimedSpaceKey) => {
      val curORSet = c.get(TimedSpaceKey)
      val curSpaceStrings = curORSet.elements
      val curTimedSpaces = curSpaceStrings.map(OID(_))
      AdminInternal.setTimedSpaces(curTimedSpaces)
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

/**
 * This is a tiny worker Actor, which deals with the long-running operation of fetching *all* of the
 * userIds in the system.
 * 
 * TODO: this is clearly broken. Rewrite this using akka-stream, to stream the userIds into something.
 */
private [admin] class AdminUserIdFetcher(val ecology:Ecology) extends Actor with EcologyMember {
  
  import AdminActor._
  
  lazy val UserAccess = interface[querki.identity.UserAccess]

  def receive = {    
    case GetAllUserIdsForAdmin(req) => req.requireAdmin {
      val userIds = UserAccess.getAllIdsForAdmin(req)
      sender ! AllUserIds(userIds)
      context.stop(self)
    }
  }
}

private[admin] object AdminActor {
  case object StatusTimeout
  
  case class SendSystemMessage(req:User, header:String, body:String)
  
  /**
   * Note that only Admins are allowed to call this!
   * 
   * TODO: this is a scalability bug! We need to come up with more stream-compatible ways to do this!
   * This will fail horribly once we're past a few thousand users!
   */
  case class GetAllUserIdsForAdmin(req:User)  
  case class AllUserIds(users:Seq[UserId])
}