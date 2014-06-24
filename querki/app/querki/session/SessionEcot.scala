package querki.session

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import querki.ecology._
import querki.identity.User
import querki.util.ActorHelpers._

import UserSession._

private object MOIDs extends EcotIds(47)

class SessionEcot(e:Ecology) extends QuerkiEcot(e) with Session {
  /**
   * The one true handle to the Space Management system.
   */
  var _ref:Option[ActorRef] = None
  lazy val sessionManager = _ref.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new UserSessionManager(ecology)), "UserSessionManager")
  }
  
  /**************************************************
   * Implementation of the Session interface
   **************************************************/
  
  implicit val timeout = Timeout(5 seconds)
  
  def getSessionInfo(user:User):Future[UserSessionInfo] = {
    val fut = sessionManager ? FetchSessionInfo(user.id)
    fut.mapTo[UserSessionInfo]
  }
}