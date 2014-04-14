package querki.admin

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.util.Timeout

import play.api.libs.concurrent.Execution.Implicits._

import querki.ecology._
import querki.identity.User
import querki.spaces.messages.{GetSpacesStatus, SpaceStatus}

case class SystemStatus(spaces:Seq[SpaceStatus])

class AdminEcot(e:Ecology) extends QuerkiEcot(e) with EcologyMember with AdminOps {
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
  
  def getSpacesStatus[B](req:User)(cb: SystemStatus => B):Future[B] = {
    akka.pattern.ask(adminActor, GetSpacesStatus(req))(Timeout(5 seconds)).mapTo[SystemStatus].map(cb)
  }
}