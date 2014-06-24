package querki.session

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout

import querki.ecology._
import querki.identity.UserId
import querki.spaces.SpacePersistenceFactory
import querki.util._

class UserSessionManager(val ecology:Ecology) extends Actor with EcologyMember with RoutingParent[UserId] {
  
  def createChild(key:UserId):ActorRef = context.actorOf(UserSession.actorProps(ecology, key).withDispatcher("session-dispatcher"), key.toString)

  def receive = LoggingReceive {
    // The main job here is to route to the children:
    case msg:UserSession.UserSessionMsg => routeToChild(msg.userId, msg)
  }
}
