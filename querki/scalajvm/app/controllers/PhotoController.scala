package controllers

import javax.inject._
import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.{ByteString, Timeout}
import upickle.default._
import play.api.libs.streams.Accumulator
import play.api.mvc._
import models.OID
import querki.globals._
import querki.photos.PhotoUploadMessages._
import querki.spaces.messages.BeginProcessingPhoto
import querki.streaming.UploadMessages.ProcessUpload
import querki.util.QLog

class PhotoController @Inject() (
  implicit
  val appProv: Provider[play.api.Application],
  mat: Materializer,
  val controllerComponents: ControllerComponents
) extends ApplicationBase
     with StreamController {

  def photoReceiver(
    ownerIdStr: String,
    spaceIdStr: String
  )(
    rh: RequestHeader
  ): Accumulator[ByteString, Either[Result, ActorRef]] = {
    implicit val timeout = Timeout(5.seconds)

    def produceUploadLocation: Future[ActorRef] = {
      for {
        ownerId <- getOwnerIdentity(ownerIdStr)
        spaceId <- SpaceOps.getSpaceId(ownerId, spaceIdStr)
        workerRef <- (SpaceOps.spaceRegion ? BeginProcessingPhoto(
          querki.identity.User.Anonymous,
          spaceId,
          rh.contentType
        )).mapTo[ActorRef]
      } yield workerRef
    }

    uploadBodyChunks(produceUploadLocation)(rh)
  }

  def upload(
    ownerId: String,
    spaceId: String,
    thingIdStr: String
  ): EssentialAction =
    withUser(true, parser = BodyParser(photoReceiver(ownerId, spaceId) _)) { rc =>
      // By the time we get here, we have fired up a PhotoUploadActor, and we have a pointer to a Future that
      // will be completed once all of the chunks are sent to it:
      logTrace(s"Finished receiving")

      // TODO: this should really check headOption, and give some more-meaningful error if it is empty:
      val propIdStr = rc.queryParam("propId").head
      val propId = OID(propIdStr)
      val thingId = OID(thingIdStr)

      // This Future will give us the pointer to the PhotoUploadActor once we're done sending stuff to it:
      val workerRef = rc.request.body
      // Tell the Actor that we're done sending it bytes, and it's time to process:
      workerRef.ask(ProcessUpload(PhotoUploadMetadata(rc, propId, thingId)))(30.seconds).map {
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
