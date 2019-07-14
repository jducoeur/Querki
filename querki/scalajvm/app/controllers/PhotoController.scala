package controllers

import javax.inject._

import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import upickle.default._
import play.api.libs.streams.Streams
import play.api.mvc._
import models.{OID, Wikitext}
import querki.globals._
import querki.photos.PhotoUploadMessages._
import querki.spaces.messages.BeginProcessingPhoto
import querki.util.QLog
import querki.values.RequestContext

class PhotoController @Inject() (val appProv:Provider[play.api.Application]) extends ApplicationBase with StreamController {
  def photoReceiver(ownerIdStr:String, spaceIdStr:String)(rh:RequestHeader) = {
    implicit val timeout = Timeout(5 seconds)
    def produceUploadLocation:Future[ActorRef] = {
      for {
        ownerId <- getOwnerIdentity(ownerIdStr)
        spaceId <- SpaceOps.getSpaceId(ownerId, spaceIdStr)
        // TODO: there's a bug here -- we are initiating Photo upload before we actually validate the uploading User.
        // I don't think it's critical, but it should be looked into.
        rc = RequestContext(None, ownerId, Some(spaceIdStr))
        workerRef <- (SpaceOps.spaceRegion ? BeginProcessingPhoto(rc, spaceId, rh.contentType)).mapTo[ActorRef]
      }
        yield workerRef
    }
    
    Streams.iterateeToAccumulator(uploadBodyChunks(produceUploadLocation)(rh))
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
          val pickled = write(wikitext)
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
