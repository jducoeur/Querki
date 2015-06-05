package controllers

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import upickle._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.libs.iteratee._
import play.api.mvc._

import models.{OID, Wikitext}

import querki.core.QLText
import querki.globals._
import querki.photos.PhotoUploadMessages._
import querki.session.messages.ChangeProps2
import querki.spaces.messages.{SessionRequest, ThingError, ThingFound}
import querki.util.QLog
import querki.values.QLContext

class PhotoController extends ApplicationBase {
  
  lazy val Core = interface[querki.core.Core]
  lazy val Photos = interface[querki.photos.Photos]
  lazy val QL = interface[querki.ql.QL]
  
  def photoReceiver(rh:RequestHeader):Iteratee[Array[Byte], Either[Result, Future[ActorRef]]] = {
    val workerRefFuture = Photos.createWorker(rh.contentType)
    Iteratee.fold[Array[Byte], Future[ActorRef]](workerRefFuture) { (fut, bytes) => 
      val newFut = fut.map { ref => ref ! PhotoChunk(bytes); ref }
      newFut
    } map (fut => Right(fut))
  }

  // TODO: in general, this all needs a rework to function correctly in the clustered world. withThing() is becoming illegal.
  // This code probably all needs to move into UserSpaceState, at least as much as possible:
  def upload(ownerId:String, spaceId:String, thingId:String) = withThing(true, ownerId, spaceId, thingId, parser = BodyParser(photoReceiver _)) { rc =>
    QLog.spew(s"Finished receiving")
    // TODO: this should really check headOption, and give some more-meaningful error if it is empty:
    val propIdStr = rc.queryParam("propId").head
    val propId = OID(propIdStr)
    implicit val s = rc.state.get
    val prop = s.prop(propId).getOrElse(throw new Exception("Attempting to upload unknown Property " + propId))
    val existingValOpt = rc.thing.flatMap(_.getPropOpt(propId)).map(_.v)
    
    val body = rc.request.body.asInstanceOf[Future[ActorRef]]
    body flatMap { workerRef =>
      val resultsFut = workerRef.ask(UploadDone(existingValOpt, prop, s))(30 seconds).mapTo[PhotoInfo]
      // TODO: all of this needs to move into the Actors themselves! This is passing state around, which is a no-no!
      resultsFut.flatMap { info =>
        QLog.spew(s"About to actually update the Space -- the QValue is ${info.newValue}")
        askSpace(rc.ownerId, spaceId)(SessionRequest(rc.requesterOrAnon, rc.ownerId, _, ChangeProps2(rc.thing.get.id.toThingId, Map((propId -> info.newValue))))) {
          case ThingFound(thingId, newState) => {
            // Okay, we're successful. Send the Wikitext for thumbnail of the new photo back to the Client:
            val lastElem = info.newValue.cv.last
            val wikified:Wikitext = QL.process(QLText("[[_thumbnail]]"), QLContext(Core.ExactlyOne(lastElem), Some(rc)))
            val pickled = upickle.write(wikified)
            Ok(pickled)
          }
          case ThingError(error, stateOpt) => {
            // TODO: do more here...
            NotAcceptable("Something went wrong!")
          }
        }
      }
    }
  }

}
