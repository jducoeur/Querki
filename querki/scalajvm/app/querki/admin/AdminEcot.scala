package querki.admin

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor._
import akka.contrib.pattern._
import akka.pattern._
import akka.util.Timeout

import play.api.libs.concurrent.Execution.Implicits._

import models.Wikitext

import querki.core.QLText
import querki.ecology._
import querki.identity.{Identity, PublicIdentity, User, UserId}
import querki.notifications._
import querki.spaces.messages.{GetSpacesStatus, SpaceStatus}
import querki.time.DateTime
import querki.values.QLContext

case class SystemStatus(spaces:Seq[SpaceStatus])

private[admin] object MOIDs extends EcotIds(43) {
  val HeaderOID = moid(1)
  val BodyOID = moid(2)
}

private[admin] trait AdminInternal extends EcologyInterface {
  def createMsg(from:PublicIdentity, header:String, body:String):Notification
}

class AdminEcot(e:Ecology) extends QuerkiEcot(e) with EcologyMember with AdminOps with AdminInternal {
  import AdminActor._
  import MOIDs._
  
  lazy val NotifierRegistry = interface[querki.notifications.NotifierRegistry]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  /**
   * The one true handle to the Admin Actor, which deals with asynchronous communications with the other Actors.
   */
  var _ref:Option[ActorRef] = None
  lazy val adminActor = _ref.get
  
  var _monitorManager:Option[ActorRef] = None
  lazy val monitorManager = _monitorManager.get
  var _monitorProxy:Option[ActorRef] = None
  lazy val monitor = _monitorProxy.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _ref = createActorCb(Props(classOf[AdminActor], ecology), "Admin")
    
    _monitorManager = createActorCb(ClusterSingletonManager.props(
        AdminMonitor.actorProps(ecology),
        "AdminMonitor",
        PoisonPill,
        None
      ), 
      "MonitorManager")
    _monitorProxy = createActorCb(ClusterSingletonProxy.props(
        "/user/querkiRoot/MonitorManager/AdminMonitor",
        None),
      "MonitorProxy")
  }
  
  override def postInit() = {
    NotifierRegistry.register(SystemMessageNotifier)
    ApiRegistry.registerApiImplFor[AdminFunctions, AdminFunctionsImpl](monitor, true)
  }
  
  override def term() = {
    NotifierRegistry.unregister(SystemMessageNotifier)
  }
  
  def getSpacesStatus[B](req:User)(cb: SystemStatus => B):Future[B] = {
    akka.pattern.ask(adminActor, GetSpacesStatus(req))(Timeout(5 seconds)).mapTo[SystemStatus].map(cb)
  }
  
  def sendSystemMessage(req:User, header:String, body:String) = {
    adminActor ! SendSystemMessage(req, header:String, body:String)
  }
  
  def getAllUserIds(req:User):Future[Seq[UserId]] = {
    // Very long timeout for this one, becaue it really might take a long time:
    implicit val timeout = Timeout(1 minute)
    val fut = adminActor ? GetAllUserIdsForAdmin(req)
    fut.mapTo[AllUserIds].map { _.users }
  }
  
  object Notifiers {
    val SystemMessage:Short = 1
  }
  
  object SystemMessageNotifier extends Notifier {
    def id = NotifierId(MOIDs.ecotId, Notifiers.SystemMessage)
  
    // System Messages don't get summarized -- they are infrequent and important:
    def summarizeAt:SummarizeAt.SummarizeAt = SummarizeAt.None
  
    def summarizeNew(context:QLContext, notes:Seq[Notification]):Future[SummarizedNotifications] = {
      if (notes.length != 1)
        throw new Exception("SystemMessageNotifier.summarizeNew expects exactly one notification at a time!")
      
      val note = notes.head
      render(context, note) map { rendered =>
        SummarizedNotifications(rendered.headline, rendered.content, notes)        
      }
    }
    
    def render(context:QLContext, note:Notification):Future[RenderedNotification] = {
      val payload = note.payload
      val futuresOpt = for {
        headerQV <- payload.get(HeaderOID)
        headerQL <- headerQV.firstAs(LargeTextType)
        header = QL.process(headerQL, context)
        bodyQV <- payload.get(BodyOID)
        bodyQL <- bodyQV.firstAs(TextType)
        body = QL.process(bodyQL, context)
      }
        yield (header, body)
        
      futuresOpt match {
        case Some((headerFut, bodyFut)) => {
          for {
            header <- headerFut
            body <- bodyFut
          }
            yield RenderedNotification(header, body)
        }
        case None => throw new Exception("SystemMessageNotifier received badly-formed Notification?")
      }
    }
  }
    
  def createMsg(from:PublicIdentity, header:String, body:String):Notification = {
    val payload = toProps(SystemMsgHeader(header), SystemMsgBody(body))
    Notification(
      EmptyNotificationId,
      from.id, 
      None,
      SystemMessageNotifier.id,
      DateTime.now,
      None, 
      None, 
      payload)
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val SystemMsgHeader = new SystemProperty(HeaderOID, LargeTextType, ExactlyOne,
    toProps(
      setName("_systemMessageHeader"),
      setInternal))
  
  lazy val SystemMsgBody = new SystemProperty(BodyOID, TextType, ExactlyOne,
    toProps(
      setName("_systemMessageBody"),
      setInternal))
  
  override lazy val props = Seq(
    SystemMsgHeader,
    SystemMsgBody
  )
}