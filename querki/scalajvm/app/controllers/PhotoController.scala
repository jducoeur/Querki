package controllers

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import upickle._

import play.api.libs.iteratee._
import play.api.mvc._

import models.{OID, Wikitext}

import querki.globals._
import querki.photos.PhotoUploadMessages._
import querki.spaces.messages.BeginProcessingPhoto
import querki.util.QLog

class PhotoController extends ApplicationBase {

  /**
   * This is the most subtle and important plumbing in the process of uploading Photos. This is a BodyParser,
   * which means that it produces an Iteratee that accept Byte Arrays (chunks of photo), and returns a "body"
   * that is actually a Future[ActorRef].
   * 
   * What happens in here is that it sends BeginProcessingPhoto to the specified Space. That causes a
   * PhotoUploadActor to be created, and we get back an ActorRef to it. We then create a fancy Iteratee that
   * keeps folding over the input chunks, producing a new Future each time. These Futures always contains the
   * same thing -- the ActorRef -- but we don't get to the final one until we've processed all of the bytes.
   */
  def photoReceiver(ownerIdStr:String, spaceIdStr:String)(rh:RequestHeader):Iteratee[Array[Byte], Either[Result, Future[ActorRef]]] = {
    implicit val timeout = Timeout(5 seconds)

    val workerRefFuture:Future[ActorRef] = for {
      ownerId <- getOwnerIdentity(ownerIdStr)
      spaceId <- SpaceOps.getSpaceId(ownerId, spaceIdStr)
      response <- (SpaceOps.spaceRegion ? BeginProcessingPhoto(querki.identity.User.Anonymous, spaceId, rh.contentType)).mapTo[ActorRef]
    }
      yield response
      
    // Now, fold over all of the chunks, producing a new Future each time
    // TODO: we ought to put a limit on the total number of bytes we are willing to send here, to avoid
    // DOS attacks:
    Iteratee.fold[Array[Byte], Future[ActorRef]](workerRefFuture) { (fut, bytes) => 
      val newFut = fut.map { ref => ref ! PhotoChunk(bytes); ref }
      newFut
    } map (fut => Right(fut))
  }

  def upload(ownerId:String, spaceId:String, thingIdStr:String) = 
    withThing(true, ownerId, spaceId, thingIdStr, parser = BodyParser(photoReceiver(ownerId, spaceId) _)) 
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
