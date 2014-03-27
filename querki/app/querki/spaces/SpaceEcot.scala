package querki.spaces

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor.ActorRef
import akka.util.Timeout

// TODO: this is a very unfortunate layer break, but is needed to provide the execution context for
// sending asks to the SpaceManager. Can/should we wrap it in something?
import play.api.libs.concurrent.Execution.Implicits._

import querki.ecology._
import querki.spaces.messages._

object SpaceEcotMOIDs extends EcotIds(37) {}

class SpaceEcot(e:Ecology) extends QuerkiEcot(e) with SpaceOps {
  /**
   * The one true handle to the Space Management system.
   */
  var _ref:Option[ActorRef] = None
  def setSpaceManager(r:ActorRef) = { _ref = Some(r) }
  lazy val spaceManager = _ref.get
    
  def askSpaceManager[A,B](msg:SpaceMgrMsg)(cb: A => B)(implicit m:Manifest[A]):Future[B] = {
    akka.pattern.ask(spaceManager, msg)(Timeout(5 seconds)).mapTo[A].map(cb)
  }
  
  def askSpaceManager2[B](msg:SpaceMgrMsg)(cb: PartialFunction[Any, B]):Future[B] = {
    akka.pattern.ask(spaceManager, msg)(Timeout(5 seconds)).map(cb)
  }
}