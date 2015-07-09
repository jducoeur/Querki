package querki.session

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.reflect.ClassTag
import java.lang.reflect.Constructor
import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.pattern.ask
import akka.util.Timeout
import querki.ecology._
import querki.identity.{Identity, PublicIdentity, User}
import messages._
import querki.util.ActorHelpers._
import UserSessionMessages._
import querki.notifications.NotificationFunctionsImpl

private object MOIDs extends EcotIds(47)

class SessionEcot(e:Ecology) extends QuerkiEcot(e) with Session with SessionHandlerRegistry with SessionInvocation {
  val SystemManagement = initRequires[querki.system.SystemManagement]
  
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
    case msg @ ClientRequest(req, rc) => (rc.requesterOrAnon.id.raw % 30).toString()
    case msg:UserSessionMsg => (msg.userId.raw % 30).toString()
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _ref = Some(ClusterSharding(SystemManagement.actorSystem).start(
        typeName = "User", 
        entryProps = Some(UserSession.actorProps(ecology)), 
        idExtractor = idExtractor, 
        shardResolver = shardResolver))
  }
  
  override def postInit() = {
    registerUserSessionImplFor[querki.api.ThingFunctions, ThingFunctionsImpl]
    registerUserSessionImplFor[querki.notifications.NotificationFunctions, NotificationFunctionsImpl]
  }
  
  /**************************************************
   * Implementation of the Session interface
   **************************************************/
  
  implicit val timeout = Timeout(5 seconds)
  
  def getCollaborators(user:User, identity:Identity, term:String):Future[Collaborators] = {
    val fut = sessionManager ? GetCollaborators(user.id, identity.id, term)
    fut.mapTo[Collaborators]
  }

  /**************************************************
   * Implementation of the Session and SessionInvocation interfaces
   **************************************************/
  
  /**
   * Map from API classes to the constructors for their handlers.
   */
  var sessionHandlers = Map.empty[String, Constructor[AutowireApiImpl]]

  def registerUserSessionImplFor[API, IMPL <: API with AutowireApiImpl](implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL]) = {
    val api = apiTag.runtimeClass
    val apiName = api.getName
    val impl = implTag.runtimeClass
    // This asInstanceOf is sad, but for some reason it seems to be losing the constructed type otherwise.
    // (Some odd erasure behavior?)
    val constr = impl.getConstructor(classOf[AutowireParams], classOf[Ecology]).asInstanceOf[Constructor[AutowireApiImpl]]
    sessionHandlers += (apiName -> constr)
  }
  
  def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams) = {
    // NOTE: we explicitly presume that the function is simply named under the API class. Is this always true?
    val apiName = req.path.dropRight(1).mkString(".")
    sessionHandlers.get(apiName) match {
      case Some(constr) => constr.newInstance(params, ecology).handleRequest(req)
      case None => throw new Exception(s"handleSessionRequest got request for unknown API $apiName")
    }
  }
}