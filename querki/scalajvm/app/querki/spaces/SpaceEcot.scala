package querki.spaces

import scala.concurrent.duration._

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.pattern._
import akka.util.Timeout

import models._
import Thing._

import querki.api.ClientRequest
import querki.core.NameUtils
import querki.ecology._
import querki.globals._
import querki.spaces.messages._
import querki.util.PublicException
import querki.util.ActorHelpers._
import querki.values.QLContext

object SpaceEcotMOIDs extends EcotIds(37) {
  val CreateHereOID = moid(1)
}

class SpaceEcot(e:Ecology) extends QuerkiEcot(e) with SpaceOps with querki.core.MethodDefs {
  
  import SpaceEcotMOIDs._
  
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
  
  def spaceIdGuts[R](ownerId:OID, spaceId:String, onSuccess:OID => R, onFailure:ThingError => R):Future[R] = {
    ThingId(spaceId) match {
      case AsOID(oid) => Future.successful(onSuccess(oid))
      case AsName(name) => {
        val canonName = NameUtils.canonicalize(name)
        ask(spaceManager, PersistMessages.GetSpaceByName(ownerId, canonName)).map {
          case SpaceId(spaceId) => onSuccess(spaceId)
          case err:ThingError => onFailure(err)
        }
      }
    }
  }
  
  def getSpaceId(ownerId:OID, spaceId:String):Future[OID] = {
    spaceIdGuts(ownerId, spaceId, oid => oid, err => throw new PublicException("Space.get.notFound", spaceId))
  }
  
  def spaceExists(ownerId:OID, spaceId:String):Future[Boolean] = {
    spaceIdGuts(ownerId, spaceId, oid => true, err => false)
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

  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  // TODO: this function needs to be rewritten as asynchonrous; at the moment, it does an evil
  // blocking function!
  lazy val CreateHere = new InternalMethod(CreateHereOID,
    toProps(
      setName("_createHere"),
      Summary("Create a nw Thing as part of displaying this expression"),
      Details("""```
        |MODEL -> LINK PROPERTY._createHere -> THING
        |```
        |
        |This allows you to create a new Thing, right here on the page, as part of 
        |displaying. It is useful to put inside of a _QLButton, so that you can create something
        |and edit it, on the press of a button, without changing pages.""".stripMargin)))
  {
    override def qlApplyTop(inv:Invocation, transformThing:Thing):Future[QLContext] = {
      // Need to shortcut with some mutation, since we don't have a good way to get this
      // side-effect out:
      var newState:Option[SpaceState] = None
      val vFut:QFut = for {
        model <- inv.contextFirstThing
        initialProps <- inv.fut(getInitialProps(inv))
        msg = CreateThing(inv.context.request.requesterOrAnon, inv.context.state, Kind.Thing, model.id, initialProps)
        newThingId <- inv.fut(spaceRegion ? msg map { 
          case ThingFound(id, s) => {
            newState = Some(s)
            id
          }
        })
      }
        yield ExactlyOne(LinkType(newThingId))
        
      vFut.map(v => inv.context.nextFrom(v, transformThing).withState(newState.get))
    }
    
    def getInitialProps(inv:Invocation):Future[PropMap] = {
      val linkBack = for {
        linkPropOpt <- inv.definingContextAsOptionalPropertyOf(LinkType)
        linkProp <- inv.opt(linkPropOpt)
        // It's important to create the new value using the correct Collection, since that's about to
        // get stored:
        ctype = linkProp.cType
        lexicalThing <- inv.opt(inv.lexicalThing match { case Some(t:Thing) => Some(t); case _ => None })
       }
        yield Map(linkProp(ctype(LinkType(lexicalThing))))
        
      linkBack.get.map(_.headOption.getOrElse(emptyProps))
    }
  }
  
  override lazy val props = Seq(
    CreateHere
  )
}
