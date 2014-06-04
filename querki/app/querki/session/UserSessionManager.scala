package querki.session

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import querki.ecology._

class UserSessionManager(val ecology:Ecology) extends Actor with EcologyMember {

  def receive = {
    // TODO: remove this, and put in real stuff:
    case _ => ???
  }
}