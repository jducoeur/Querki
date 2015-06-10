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
import querki.spaces.messages.{BeginProcessingPhoto, SessionRequest, ThingError, ThingFound}
import querki.util.QLog
import querki.values.QLContext

class PhotoController extends ApplicationBase {
  
  lazy val Core = interface[querki.core.Core]
  lazy val QL = interface[querki.ql.QL]
  
  def photoReceiver(ownerIdStr:String, spaceIdStr:String)(rh:RequestHeader):Iteratee[Array[Byte], Either[Result, Future[ActorRef]]] = {
    implicit val timeout = Timeout(5 seconds)
    
    val workerRefFuture = for {
      ownerId <- getOwnerIdentity(ownerIdStr)
      spaceId <- SpaceOps.getSpaceId(ownerId, spaceIdStr)
      response <- (SpaceOps.spaceRegion ? BeginProcessingPhoto(querki.identity.User.Anonymous, spaceId, rh.contentType)).mapTo[ActorRef]
    }
      yield response
      
    Iteratee.fold[Array[Byte], Future[ActorRef]](workerRefFuture) { (fut, bytes) => 
      val newFut = fut.map { ref => ref ! PhotoChunk(bytes); ref }
      newFut
    } map (fut => Right(fut))
  }

  // TODO: in general, this all needs a rework to function correctly in the clustered world. withThing() is becoming illegal.
  // This code probably all needs to move into UserSpaceState, at least as much as possible:
  def upload(ownerId:String, spaceId:String, thingIdStr:String) = withThing(true, ownerId, spaceId, thingIdStr, parser = BodyParser(photoReceiver(ownerId, spaceId) _)) { rc =>
    QLog.spew(s"Finished receiving")
    // TODO: this should really check headOption, and give some more-meaningful error if it is empty:
    val propIdStr = rc.queryParam("propId").head
    val propId = OID(propIdStr)
    val thingId = OID(thingIdStr)
    
    val refFut = rc.request.body.asInstanceOf[Future[ActorRef]]
    refFut flatMap { workerRef =>
      workerRef.ask(UploadDone(rc, propId, thingId))(30 seconds).map {
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
