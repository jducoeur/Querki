package querki.admin

import scala.concurrent.duration._

import akka.actor._

import querki.ecology._
import querki.spaces.messages.{GetSpacesStatus, SpaceStatus}

case object StatusTimeout

private[admin] class AdminActor(val ecology:Ecology) extends Actor with EcologyMember {
  
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
  }
  
}