package querki.notifications

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.util.Timeout

import play.api.libs.concurrent.Execution.Implicits._

import querki.ecology._
import querki.identity.{PublicIdentity, User}
import querki.values.{QLRequestContext, RequestContext}

private object MOIDs extends EcotIds(48)

class NotificationEcot(e:Ecology) extends QuerkiEcot(e) with NotifierRegistry with Notifications {
  
  import NotificationActor._
  
  /**
   * The one true handle to the Notifications Actor.
   */
  var _ref:Option[ActorRef] = None
  lazy val noteActor = _ref.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new NotificationActor(ecology)), "Notifications")
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
  
  def send(req:User, as:PublicIdentity, recipients:Recipients, note:Notification) = {
    noteActor ! SendNotification(req, as, recipients, note)
  }
  
  def render(rc:RequestContext, note:Notification):RenderedNotification = {
    val notifier = notifiers(note.notifier)
    val context = QLRequestContext(rc)
    notifier.render(context, note)
  }
}