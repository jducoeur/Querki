package querki.session

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.event.LoggingReceive
import models._
import querki.admin.{SpaceMonitorEvent, MonitorActor}
import querki.api.ClientRequest
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.publication.CurrentPublicationState
import querki.session.messages._
import querki.spaces.TracingSpace
import querki.spaces.messages.{SpaceSubsystemRequest, CurrentState}
import querki.util.{QuerkiBootableActor, RoutingParent}
import querki.values._

private [session] class UserSpaceSessions(e:Ecology, val spaceId:OID, val spaceRouter:ActorRef, timeSpaceOps:Boolean)
  extends QuerkiBootableActor(e) with EcologyMember with ReceivePipeline with RoutingParent[User]
{
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Publication = interface[querki.publication.Publication]
  lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  val persister = SpacePersistenceFactory.getUserValuePersister(spaceId)

  lazy val tracing = TracingSpace(spaceId, "UserSpaceSessions: ")
  
  var state:Option[SpaceState] = None
  var pubState:Option[CurrentPublicationState] = None

  // For the moment, UserSpaceSessions is responsible for keeping the AdminMonitor apprised of the state
  // of this Space:
  lazy val monitor = context.actorOf(MonitorActor.actorProps(ecology))
  override def childrenUpdated() = {
    tracing.trace("childrenUpdated")
    state.foreach(s => monitor ! SpaceMonitorEvent(spaceId, s.displayName, SystemManagement.clusterAddress, nChildren, s.spaceSize))
  }
  
  def createChild(key:User):ActorRef = {
    tracing.trace(s"createChild(${key.id})")
    // Sessions need a special dispatcher so they can use Stash. (Seriously? Unfortunate leakage in the Akka API.)
    context.actorOf(OldUserSpaceSession.actorProps(ecology, spaceId, key, spaceRouter, persister, timeSpaceOps).withDispatcher("session-dispatcher"), key.id.toString)
  }
  
  def sendPublishStateToChild(user:User, session:ActorRef):Unit = {
    tracing.trace(s"sendPublishStateToChild(${user.id})")
    // TODO: this is nasty and coupled -- we are checking whether this user has publication access:
    (state, pubState) match {
      case (Some(s), Some(ps)) => {
        if (AccessControl.hasPermission(Publication.CanPublishPermission, s, user, s.id))
          session ! ps
      }
      case _ =>
    }
  }
  
  /**
   * Send the CurrentPublicationState to children who have access to it. This will only do anything if
   * we have *both* the the normal and publication states; otherwise it is a no-op.
   */
  def sendPublishStateToChildren():Unit = {
    childPairs.map { case (user, child) => sendPublishStateToChild(user, child.ref) }
  }
  
  def isPublishableSpace(state:SpaceState):Boolean = {
    implicit val s = state
    state.ifSet(Publication.SpaceHasPublications)
  }
  
  override def initChild(user:User, child:ActorRef) = {
    tracing.trace(s"initChild(${user.id})")
    sendPublishStateToChild(user, child)
    state.map(child ! CurrentState(_))
  }
  
  def bootIfReady() = {
    tracing.trace(s"bootIfReady")
    // We always need the SpaceState in order to boot; if the Space is Publishable, then we
    // also need the CurrentPublicationState. If the Space is not Publishable, we'll eventually
    // get a probably-empty CurrentPublicationState, but we don't have to wait for it.
    if (state.isDefined && (!isPublishableSpace(state.get) || pubState.isDefined)) {
      childrenUpdated()
      // Note that we intentionally send the PublicationState *first*, so that we don't get
      // a UserSpaceSession that is missing it due to race conditions.
      sendPublishStateToChildren()
      children.foreach { session => 
        session ! CurrentState(state.get)
      }
      
      doneBooting()
    }
  }
  
  /**
   * We stay in Boot mode until we receive *both* the SpaceState and the CurrentPublicationState.
   */
  def bootReceive = {
    case msg @ CurrentState(s, _) => {
      state = Some(s)
      bootIfReady()
    }
    
    case msg:CurrentPublicationState => {
      tracing.trace("bootReceive: CurrentPublicationState")
      pubState = Some(msg)
      bootIfReady()
    }
  }
  
  /**
   * Normal steady-state receive, after we have both the SpaceState and CurrentPublicationState.
   */
  def doReceive = {
    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(s, _) => {
      tracing.trace("doReceive: CurrentState")
      state = Some(s)
      children.foreach(session => session.forward(msg))
    }
    
    /**
     * There is a new Publication State, so tell the *right* people about it.
     */
    case msg:CurrentPublicationState => {
      tracing.trace("doReceive: CurrentPublicationState")
      pubState = Some(msg)
      sendPublishStateToChildren()
    }
    
    case GetActiveSessions => sender ! ActiveSessions(children.size)

    /**
     * Message to forward to a UserSpaceSession. Create the session, if needed.
     */
    case msg @ SpaceSubsystemRequest(requester, _, payload) => {
      tracing.trace(s"doReceive: SpaceSubsystemRequest(${payload.getClass.getSimpleName})")
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
