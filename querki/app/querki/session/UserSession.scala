package querki.session

import akka.actor._
import akka.event.LoggingReceive

import models.{AsName, AsOID, OID, UnknownOID}

import querki.ecology._
import querki.session.messages._
import querki.spaces.messages.{CurrentState, ThingError, ThingFound}
import querki.spaces.messages.SpaceError._
import querki.util.PublicException
import querki.values.SpaceState

private [session] class UserSession(val ecology:Ecology, val spaceId:OID)
  extends Actor with Stash with EcologyMember
{
  
  /**
   * IMPORTANT: this must be set before we begin any serious work! This is why we start
   * in a rudimentary state, and don't become useful until it is received.
   */
  var _state:Option[SpaceState] = None
  def state = _state.get

  /**
   * Initial state: stash everything until we get the SpaceState. CurrentState will *typically* come first, but
   * might not in cases where the SessionRequest is bootstrapping this hive, or comes in while bootstrap is still
   * in process.
   * 
   * Note that this state only persists until we get a SpaceState, at which point we switch to normalReceive, and
   * stay there for the rest of this actor's life.
   */
  def receive = LoggingReceive {
    case CurrentState(s) => {
      _state = Some(s)
      unstashAll()
      context.become(normalReceive)
    }
    
    case _ => stash()
  }
  
  def normalReceive:Receive = LoggingReceive {
    case CurrentState(s) => _state = Some(s)
    
    case GetThing(thingIdOpt) => {
      // TODO: enhance the returned state with the UserValues for this Thing, if any. Actually,
      // we may need to maintain a fully-enhanced version of the state, to make QL expressions look
      // correct for this user.
      val thingId = thingIdOpt.flatMap(state.anything(_)).map(_.id).getOrElse(UnknownOID)
      if (thingIdOpt.isDefined) {
        val thingOpt = state.anything(thingIdOpt.get)
        if (thingOpt.isDefined) {
          sender ! ThingFound(thingOpt.get.id, state)
        } else {
          thingIdOpt.get match {
            // TODO: this potentially leaks information. It is frequently legal to see the Space if the name is unknown --
            // that is how tags work -- but we should think through how to keep that properly controlled.
            case AsName(name) => sender ! ThingError(new PublicException(UnknownName), Some(state))
            case AsOID(id) => sender ! ThingError(new PublicException(UnknownID))
          }
        }
      } else {
        // TODO: is this the most semantically appropriate response?
        sender ! ThingFound(UnknownOID, state)
      }    
    }
  }
}

object UserSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID):Props = Props(new UserSession(ecology, spaceId))
}
