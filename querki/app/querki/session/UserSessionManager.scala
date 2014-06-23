package querki.session

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import querki.ecology._
import querki.identity.UserId
import querki.spaces.SpacePersistenceFactory
import querki.util._

class UserSessionManager(val ecology:Ecology) extends Actor with EcologyMember with RoutingParent[UserId] {
  
  def createChild(key:UserId):ActorRef = context.actorOf(UserSession.actorProps(ecology, key), key.toString)

  def receive = {
    // TODO: remove this, and put in real stuff:
    case _ => ???
  }
}
