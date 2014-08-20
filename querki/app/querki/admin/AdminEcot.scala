package querki.admin

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.util.Timeout

import play.api.libs.concurrent.Execution.Implicits._

import models.Wikitext

import querki.core.QLText
import querki.ecology._
import querki.identity.{Identity, PublicIdentity, User}
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
  
  lazy val QL = interface[querki.ql.QL]
  lazy val NotifierRegistry = interface[querki.notifications.NotifierRegistry]
  
  /**
   * The one true handle to the Admin Actor, which deals with asynchronous communications with the other Actors.
   */
  var _ref:Option[ActorRef] = None
  lazy val adminActor = _ref.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new AdminActor(ecology)), "Admin")
  }
  
  override def postInit() = {
    NotifierRegistry.register(SystemMessageNotifier)
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
  
  object Notifiers {
    val SystemMessage:Short = 1
  }
  
  object SystemMessageNotifier extends Notifier {
    def id = NotifierId(MOIDs.ecotId, Notifiers.SystemMessage)
  
    // System Messages don't get summarized -- they are infrequent and important:
    def summarizeAt:SummarizeAt.SummarizeAt = SummarizeAt.None
  
    def summarizeNew(context:QLContext, notes:Seq[Notification]):SummarizedNotifications = {
      if (notes.length != 1)
        throw new Exception("SystemMessageNotifier.summarizeNew expects exactly one notification at a time!")
      
      val note = notes.head
      val rendered = render(context, note)
      SummarizedNotifications(rendered.headline, rendered.content, notes)
    }
    
    def render(context:QLContext, note:Notification):RenderedNotification = {
      val payload = note.payload
      val resultOpt = for {
        headerQV <- payload.get(HeaderOID)
        headerQL <- headerQV.firstAs(LargeTextType)
        header = QL.process(headerQL, context)
        bodyQV <- payload.get(BodyOID)
        bodyQL <- bodyQV.firstAs(TextType)
        body = QL.process(bodyQL, context)
      }
        yield RenderedNotification(header, body)
        
      resultOpt.getOrElse(throw new Exception("SystemMessageNotifier received badly-formed Notification?"))
    }
  }
    
  def createMsg(from:PublicIdentity, header:String, body:String):Notification = {
    val payload = toProps(SystemMsgHeader(header), SystemMsgBody(body))()
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