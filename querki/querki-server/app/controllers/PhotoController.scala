package controllers

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.libs.iteratee._
import play.api.mvc._

import models.OID

import querki.photos.PhotoUploadMessages._
import querki.session.messages.ChangeProps2
import querki.spaces.messages.{SessionRequest, ThingError, ThingFound}
import querki.util.QLog

class PhotoController extends ApplicationBase {
  
  lazy val Photos = interface[querki.photos.Photos]
  
  def photoReceiver(rh:RequestHeader):Iteratee[Array[Byte], Either[Result, Future[ActorRef]]] = {
    val workerRefFuture = Photos.createWorker
    Iteratee.fold[Array[Byte], Future[ActorRef]](workerRefFuture) { (fut, bytes) => 
      val newFut = fut.map { ref => ref ! PhotoChunk(bytes); ref }
      newFut
    } map (fut => Right(fut))
  }

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
      resultsFut.flatMap { info =>
        QLog.spew(s"About to actually update the Space -- the QValue is ${info.newValue}")
        val sessionRequest = 
          SessionRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id.toThingId, 
              ChangeProps2(rc.thing.get.id.toThingId, Map((propId -> info.newValue))))
        askSpace(sessionRequest) {
          case ThingFound(thingId, newState) => {
            Ok("Got it!")
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
