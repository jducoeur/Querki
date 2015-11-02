package querki.apps

import akka.actor._

import querki.globals._
import querki.spaces.messages._
import querki.util.QuerkiActor

/**
 * Child Actor under an App, which sends its State to a requesting Space. This implements a very
 * primitive streaming protocol for the purpose, since States can get pretty large.
 * 
 * @author jducoeur
 */
class AppSender(e:Ecology, child:ActorRef, state:SpaceState) extends QuerkiActor(e) {
  def doReceive = {
    case AppSender.Send => child ! CurrentState(state)
  }
}

object AppSender {
  case object Send
  def props(e:Ecology, child:ActorRef, state:SpaceState) = Props(classOf[AppSender], e, child, state)
}
