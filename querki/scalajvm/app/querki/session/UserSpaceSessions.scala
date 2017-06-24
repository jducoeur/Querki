package querki.session

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.event.LoggingReceive

import models.OID

import querki.admin.{MonitorActor, SpaceMonitorEvent}
import querki.api.ClientRequest
import querki.ecology._
import querki.identity.User
import querki.publication.CurrentPublicationState
import querki.session.messages._
import querki.spaces.messages.{SpaceSubsystemRequest, CurrentState}
import querki.util._
import querki.values.SpaceState

private [session] class UserSpaceSessions(val ecology:Ecology, val spaceId:OID, val spaceRouter:ActorRef, timeSpaceOps:Boolean)
  extends Actor with EcologyMember with ReceivePipeline with RoutingParent[User]
{
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Publication = interface[querki.publication.Publication]
  lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  val persister = SpacePersistenceFactory.getUserValuePersister(spaceId)
  
  var state:Option[SpaceState] = None
  var pubState:Option[SpaceState] = None

  // For the moment, UserSpaceSessions is responsible for keeping the AdminMonitor apprised of the state
  // of this Space:
  lazy val monitor = context.actorOf(MonitorActor.actorProps(ecology))
  override def childrenUpdated() = {
    state.foreach(s => monitor ! SpaceMonitorEvent(spaceId, s.displayName, SystemManagement.clusterAddress, nChildren, s.spaceSize))
  }
  
  def createChild(key:User):ActorRef = {
    // Sessions need a special dispatcher so they can use Stash. (Seriously? Unfortunate leakage in the Akka API.)
    context.actorOf(OldUserSpaceSession.actorProps(ecology, spaceId, key, spaceRouter, persister, timeSpaceOps).withDispatcher("session-dispatcher"), key.id.toString)
  }
  
  /**
   * Send the CurrentPublicationState to children who have access to it. This will only do anything if
   * we have *both* the the normal and publication states.
   */
  def sendPublishStateToChildren():Unit = {
    // TODO: this is nasty and coupled -- we are checking whether this user has publication access:
    (state, pubState) match {
      case (Some(s), Some(ps)) => {
        val msg = CurrentPublicationState(ps)
        childPairs
          .filter { case (user, _) => AccessControl.hasPermission(Publication.CanPublishPermission, s, user, s.id) }
          .map { case (user, session) => session.forward(msg) }
      }
      case _ =>
    }
  }
  
  override def initChild(child:ActorRef) = state.map(child ! CurrentState(_))
  
  def receive = LoggingReceive {
    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(s) => {
      val firstTime = state.isEmpty
      state = Some(s)
      if (firstTime) {
        childrenUpdated()
        sendPublishStateToChildren()
      }
      children.foreach(session => session.forward(msg))
    }
    
    /**
     * There is a new Publication State, so tell the *right* people about it.
     */
    case msg @ CurrentPublicationState(s) => {
      pubState = Some(s)
      sendPublishStateToChildren()
    }
    
    case GetActiveSessions => sender ! ActiveSessions(children.size)

    /**
     * Message to forward to a UserSpaceSession. Create the session, if needed.
     */
    case msg @ SpaceSubsystemRequest(requester, _, payload) => {
      payload match {
        // HACK: messages heading for the User Value Persister:
        case p:querki.uservalues.PersistMessages.ExternallyExposed => persister.forward(msg)
        case _ => routeToChild(requester, msg)
      }
    }
    case msg @ ClientRequest(req, rc) => routeToChild(rc.requesterOrAnon, msg)
  }

}

object UserSpaceSessions {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID, spaceRouter:ActorRef, timeSpaceOps:Boolean):Props = 
    Props(new UserSpaceSessions(ecology, spaceId, spaceRouter, timeSpaceOps))
}
