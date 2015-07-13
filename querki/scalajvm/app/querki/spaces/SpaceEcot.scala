package querki.spaces

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.pattern._
import akka.util.Timeout

import models.{AsName, AsOID, ThingId}

import querki.api.ClientRequest
import querki.core.NameUtils
import querki.ecology._
import querki.globals._
import Implicits._
import querki.spaces.messages._
import querki.util.PublicException

object SpaceEcotMOIDs extends EcotIds(37) {}

class SpaceEcot(e:Ecology) extends QuerkiEcot(e) with SpaceOps {
  
  val SystemManagement = initRequires[querki.system.SystemManagement]
  
  /**
   * The one true handle to the Space Management system.
   */
  var _ref:Option[ActorRef] = None
  lazy val spaceManager = _ref.get
  var _region:Option[ActorRef] = None
  lazy val spaceRegion = _region.get
  
  // These two functions tell ClusterSharding the ID and shard for a given SpaceMessage. They are
  // then used to decide how to find/create the Space's Router (and thus, its troupe).
  val idExtractor:ShardRegion.IdExtractor = {
    case msg @ ClientRequest(req, rc) => (rc.spaceIdOpt.get.toString(), msg) 
    case msg @ SpaceMessage(req, spaceId) => (spaceId.toString(), msg) 
  }
  
  val shardResolver:ShardRegion.ShardResolver = msg => msg match {
    case ClientRequest(req, rc) => rc.spaceIdOpt.get.shard
    case msg @ SpaceMessage(req, spaceId) => spaceId.shard
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _region = SystemManagement.createShardRegion("Space", SpaceRouter.actorProps(ecology), 
        idExtractor, shardResolver)
    _ref = createActorCb(Props(classOf[SpaceManager], ecology, spaceRegion), "SpaceManager")
  }
  
  implicit val stdTimeout = Timeout(10 seconds)
  
  def getSpaceId(ownerId:OID, spaceId:String):Future[OID] = {
    ThingId(spaceId) match {
      case AsOID(oid) => Future.successful(oid)
      case AsName(name) => {
        val canonName = NameUtils.canonicalize(name)
        ask(spaceManager, PersistMessages.GetSpaceByName(ownerId, canonName)).map {
          case SpaceId(spaceId) => spaceId
          // TODO: this should really be a PublicException, I think:
          case err:ThingError => throw new PublicException("Space.get.notFound", spaceId)
        }
      }
    }
  }
    
  def askSpaceManager[A,B](msg:SpaceMgrMsg)(cb: A => Future[B])(implicit m:Manifest[A]):Future[B] = {
    akka.pattern.ask(spaceManager, msg).mapTo[A].flatMap(cb)
  }
  
  def askSpaceManager2[B](msg:SpaceMgrMsg)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    akka.pattern.ask(spaceManager, msg).flatMap(cb)
  }
  
  // TODO: make this signature less idiotic:
  def askSpace[A, B](msg:SpaceMessage)(cb: A => Future[B])(implicit m:Manifest[A]):Future[B] = {
    akka.pattern.ask(spaceRegion, msg).mapTo[A].flatMap(cb)
  }
  
  def askSpace2[B](msg:SpaceMessage)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    akka.pattern.ask(spaceRegion, msg).flatMap(cb)
  }
}
