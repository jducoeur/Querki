package controllers

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import upickle._

import play.api.mvc._

import models.{OID, Wikitext}

import querki.globals._
import querki.photos.PhotoUploadMessages._
import querki.spaces.messages.BeginProcessingPhoto
import querki.util.QLog

class PhotoController extends ApplicationBase with StreamController {
  def photoReceiver(ownerIdStr:String, spaceIdStr:String)(rh:RequestHeader) = {
    implicit val timeout = Timeout(5 seconds)
    def produceUploadLocation:Future[ActorRef] = {
      for {
        ownerId <- getOwnerIdentity(ownerIdStr)
        spaceId <- SpaceOps.getSpaceId(ownerId, spaceIdStr)
        workerRef <- (SpaceOps.spaceRegion ? BeginProcessingPhoto(querki.identity.User.Anonymous, spaceId, rh.contentType)).mapTo[ActorRef]
      }
        yield workerRef
    }
    
    uploadBodyChunks(produceUploadLocation)(rh)
  }

  def upload(ownerId:String, spaceId:String, thingIdStr:String) = 
    withUser(true, parser = BodyParser(photoReceiver(ownerId, spaceId) _)) 
  { rc =>
    // By the time we get here, we have fired up a PhotoUploadActor, and we have a pointer to a Future that
    // will be completed once all of the chunks are sent to it:
    QLog.spew(s"Finished receiving")
    
    // TODO: this should really check headOption, and give some more-meaningful error if it is empty:
    val propIdStr = rc.queryParam("propId").head
    val propId = OID(propIdStr)
    val thingId = OID(thingIdStr)
    
    // This Future will give us the pointer to the PhotoUploadActor once we're done sending stuff to it:
    val refFut = rc.request.body.asInstanceOf[Future[ActorRef]]
    refFut flatMap { workerRef =>
      // Tell the Actor that we're done sending it bytes, and it's time to process:
      workerRef.ask(UploadDone(rc, propId, thingId))(30 seconds).map {
        // Once we get this, the Actor is finished, and shutting down
        case PhotoInfo(wikitext) => {
          val pickled = upickle.write(wikitext)
          Ok(pickled)
        }
        case PhotoFailed => {
          // TODO: do more here...
          NotAcceptable("Something went wrong!")          
        }
      }
    }
  }

}
