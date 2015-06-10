package querki.photos

import akka.actor._
import akka.event.LoggingReceive

import querki.ecology._

/**
 * This is a very simple Supervisor, which is used to spawn workers that deal with photo upload.
 * 
 * Note that this is *not* a RoutingParent, intentionally -- it simply create fire-and-forget workers
 * that are responsible for their own lifecycles. Each PhotoUploadActor registers itself with its
 * UserSession, and handles routing that way.
 * 
 * It is normal and expected that there be a duplicate PhotoUploadManager on each node. Each one
 * spawns PhotoUploadActors that live on that node. We do things this way to avoid passing potentially
 * multi-megabyte image files around as messages. The node that receives the HTTP upload does *all* of
 * the processing for that image.
 * 
 * TBD: can/should this be replaced by a simple Router? How would the Ecot control that?
 */
class PhotoUploadManager(val ecology:Ecology) extends Actor with EcologyMember {
  
  import PhotoUploadActor._

  def receive = LoggingReceive {
    case msg @ querki.spaces.messages.BeginProcessingPhoto(_, _, _) => {
      val worker = context.actorOf(PhotoUploadActor.actorProps(ecology))
      worker.forward(msg)
      sender ! worker
    }
  }
}
