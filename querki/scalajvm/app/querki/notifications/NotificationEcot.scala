package querki.notifications

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.util.Timeout

import play.api.libs.concurrent.Execution.Implicits._

import querki.api.ClientRequest
import querki.ecology._
import querki.globals._
import querki.identity.{PublicIdentity, User, UserRouteableMessage}
import querki.values.{QLRequestContext, RequestContext, SpaceState}

private object MOIDs extends EcotIds(48)

class NotificationEcot(e:Ecology) extends QuerkiEcot(e) with NotifierRegistry with Notifications {
  
  import NotificationActor._
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val System = interface[querki.system.System]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  /**
   * The one true handle to the Notifications Actor.
   */
  var _ref:Option[ActorRef] = None
  lazy val noteActor = _ref.get
  
  /**
   * The one true handle to the UserNotifications region.
   */
  var _userNotifications:Option[ActorRef] = None
  lazy val userNotifications = _userNotifications.get
  
  // These two functions tell ClusterSharding the ID and shard for a given UserSessionMsg.
  val idExtractor:ShardRegion.IdExtractor = {
    case msg:UserRouteableMessage => (msg.userId.toString(), msg)
    case msg @ ClientRequest(req, rc) => (rc.requesterOrAnon.id.toString(), msg)
  }
  
  val shardResolver:ShardRegion.ShardResolver = msg => msg match {
    case msg:UserRouteableMessage => msg.userId.shard
    case msg @ ClientRequest(req, rc) => rc.requesterOrAnon.id.shard
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new NotificationActor(ecology)), "Notifications")
    
    _userNotifications = SystemManagement.createShardRegion("UserNotifications", UserNotificationActor.actorProps(ecology), 
        idExtractor, shardResolver)
  }
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[NotificationFunctions, NotificationFunctionsImpl](userNotifications)    
  }

  /***********************************************
   * NotifierRegister IMPLEMENTATION
   ***********************************************/
  
  var notifiers = Map.empty[NotifierId, Notifier]

  def register(notifier:Notifier) = {
    notifiers += (notifier.id -> notifier)
  }
  
  def unregister(notifier:Notifier) = {
    notifiers -= notifier.id
  }
  

  /***********************************************
   * Notifications IMPLEMENTATION
   ***********************************************/
  
  def send(req:User, recipients:Recipients, note:Notification) = {
    noteActor ! SendNotification(req, recipients, note)
  }
  
  def render(rc:RequestContext, note:Notification):RenderedNotification = {
    val notifier = notifiers(note.notifier)
    // Note that notifications, necessarily, render against SystemState. That has to be the case,
    // because the Notifications page displays notes from many different Spaces.
    val context = QLRequestContext(rc)(System.State, ecology)
    notifier.render(context, note)
  }
}