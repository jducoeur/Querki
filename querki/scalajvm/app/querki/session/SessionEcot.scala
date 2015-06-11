package querki.session

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.pattern.ask
import akka.util.Timeout

import querki.ecology._
import querki.identity.{Identity, PublicIdentity, User}
import querki.util.ActorHelpers._

import UserSessionMessages._

private object MOIDs extends EcotIds(47)

class SessionEcot(e:Ecology) extends QuerkiEcot(e) with Session {
  val SystemManagement = initRequires[querki.system.SystemManagement]
  
  /**
   * The one true handle to the Space Management system.
   */
  var _ref:Option[ActorRef] = None
  lazy val sessionManager = _ref.get
  
  // These two functions tell ClusterSharding the ID and shard for a given UserSessionMsg.
  val idExtractor:ShardRegion.IdExtractor = {
    case msg:UserSessionMsg => (msg.userId.toString(), msg) 
  }
  
  val shardResolver:ShardRegion.ShardResolver = msg => msg match {
    case msg:UserSessionMsg => (msg.userId.raw % 30).toString()
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _ref = Some(ClusterSharding(SystemManagement.actorSystem).start(
        typeName = "User", 
        entryProps = Some(UserSession.actorProps), 
        idExtractor = idExtractor, 
        shardResolver = shardResolver))
  }
  
  /**************************************************
   * Implementation of the Session interface
   **************************************************/
  
  implicit val timeout = Timeout(5 seconds)
  
  def getSessionInfo(user:User):Future[UserSessionInfo] = {
    val fut = sessionManager ? FetchSessionInfo(user.id)
    fut.mapTo[UserSessionInfo]
  }
  
  def getNotifications(user:User):Future[RecentNotifications] = {
    val fut = sessionManager ? GetRecent(user.id)
    fut.mapTo[RecentNotifications]
  }
  
  def getCollaborators(user:User, identity:Identity, term:String):Future[Collaborators] = {
    val fut = sessionManager ? GetCollaborators(user.id, identity.id, term)
    fut.mapTo[Collaborators]
  }
}