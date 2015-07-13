package querki.session

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.pattern.ask
import akka.util.Timeout

import querki.api.ClientRequest
import querki.ecology._
import querki.globals._
import Implicits.execContext
import querki.identity.{Identity, PublicIdentity, User}
import messages._
import UserSessionMessages._
import querki.util.ActorHelpers._

private object MOIDs extends EcotIds(47)

class SessionEcot(e:Ecology) extends QuerkiEcot(e) with Session {
  val SystemManagement = initRequires[querki.system.SystemManagement]

  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]

  /**
   * The one true handle to the Space Management system.
   */
  var _ref:Option[ActorRef] = None
  lazy val sessionManager = _ref.get
  
  // These two functions tell ClusterSharding the ID and shard for a given UserSessionMsg.
  val idExtractor:ShardRegion.IdExtractor = {
    case msg @ ClientRequest(req, rc) => (rc.requesterOrAnon.id.toString(), msg)
    case msg:UserSessionMsg => (msg.userId.toString(), msg) 
  }
  
  val shardResolver:ShardRegion.ShardResolver = msg => msg match {
    case msg @ ClientRequest(req, rc) => rc.requesterOrAnon.id.shard
    case msg:UserSessionMsg => msg.userId.shard
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _ref = SystemManagement.createShardRegion("User", UserSession.actorProps(ecology), 
        idExtractor, shardResolver)
  }
  
  override def postInit() = {
    // This is the most important API for anonymous usage, so requiresLogin = false
    ApiRegistry.registerApiImplFor[querki.api.ThingFunctions, ThingFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  /**************************************************
   * Implementation of the Session interface
   **************************************************/
  
  implicit val timeout = Timeout(5 seconds)
  
  def getCollaborators(user:User, identity:Identity, term:String):Future[Collaborators] = {
    val fut = sessionManager ? GetCollaborators(user.id, identity.id, term)
    fut.mapTo[Collaborators]
  }

}