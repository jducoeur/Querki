package querki.session

import scala.concurrent.duration._

import akka.actor.{ActorRef, Props}
import akka.cluster.sharding.{ClusterSharding, ShardRegion}
import akka.pattern.ask
import akka.util.Timeout

import querki.api.ClientRequest
import querki.ecology._
import querki.globals._
import querki.identity.{Identity, PublicIdentity, User}
import messages._
import UserSessionMessages._
import querki.util.ActorHelpers._

private object MOIDs extends EcotIds(47)

class SessionEcot(e:Ecology) extends QuerkiEcot(e) with Session {
  val SystemManagement = initRequires[querki.system.SystemManagement]

  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  lazy val cassandraEnabled = Config.getBoolean("querki.cassandra.enabled", false)

  /**
   * The one true handle to UserSessions.
   */
  var _ref:Option[ActorRef] = None
  lazy val sessionManager = _ref.get
  
  // These two functions tell ClusterSharding the ID and shard for a given UserSessionMsg.
  val idExtractor:ShardRegion.ExtractEntityId = {
    case msg @ ClientRequest(req, rc) => (rc.requesterOrAnon.id.toString(), msg)
    case msg:UserSessionMsg => (msg.userId.toString(), msg) 
  }
  
  val shardResolver:ShardRegion.ExtractShardId = msg => msg match {
    case msg @ ClientRequest(req, rc) => rc.requesterOrAnon.id.shard
    case msg:UserSessionMsg => msg.userId.shard
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TEMP: only while we're in transition. Once Cassandra is firmly up, OldUserSession should
    // go away:
    val props =
      if (cassandraEnabled)
        UserSession.actorProps(ecology)
      else
        OldUserSession.actorProps(ecology)
        
    _ref = SystemManagement.createShardRegion("User", props, 
        idExtractor, shardResolver)
  }
  
  override def persistentMessages = persist(47,
    (classOf[UserSession.UserStateOld] -> 100),
    (classOf[UserSessionMessages.SetSkillLevel] -> 101),
    (classOf[UserSession.UserState] -> 102)
  )
  
  override def postInit() = {
    // This is the most important API for anonymous usage, so requiresLogin = false
    ApiRegistry.registerApiImplFor[querki.api.ThingFunctions, ThingFunctionsImpl](SpaceOps.spaceRegion, false)
    // User Functions make no sense if you aren't logged in:
    ApiRegistry.registerApiImplFor[UserFunctions, UserFunctionsImpl](sessionManager)
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