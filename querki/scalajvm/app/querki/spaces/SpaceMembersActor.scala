package querki.spaces

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.pipe

import querki.globals._

import querki.spaces.messages._

private [spaces] class SpaceMembersActor(e:Ecology, val spaceId:OID, val spaceRouter:ActorRef)
  extends Actor with Stash with EcologyMember
{
  implicit val ecology = e
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Person = interface[querki.identity.Person]
  
  lazy val maxMembers = Config.getInt("querki.public.maxMembersPerSpace", 100)

  def receive = {
    case CurrentState(state) => {
      unstashAll()
      context.become(normalReceive(state))
    }
    
    case _ => stash()
  }
  
  // Normal behavior -- at any given time, state is the current known SpaceState
  def normalReceive(state:SpaceState):Receive = {
    case CurrentState(newState) => context.become(normalReceive(newState))
    
    case SpaceMembersMessage(_, _, msg) => msg match {
  	  // Someone is attempting to join this Space:
  	  case JoinRequest(rc, personId) => {
        implicit val s = state
  	    val result:Option[Future[JoinResult]] = Person.acceptInvitation(rc, personId) {
  	      case ThingFound(id, state) => Future.successful(Joined) 
  	      case ThingError(error, stateOpt) => Future.successful(JoinFailed(error))
  	      case _ => Future.successful(JoinFailed(new PublicException("Space.join.unknownError", state.displayName)))
  	    }
  	    result match {
  	      case Some(fut) => pipe(fut) to sender
  	      case None => sender ! JoinFailed(new PublicException("Space.join.unknownError", state.displayName))
  	    }
  	  }
  	    
  	  case InviteRequest(rc, inviteeEmails, collabs) => {
  	    val nCurrentMembers = Person.people(state).size
  	    val resultFut = 
  	      if (!rc.requesterOrAnon.isAdmin && (nCurrentMembers + inviteeEmails.size + collabs.size) > maxMembers) {
  	        Future.successful(s"Sorry: at the moment you are limited to $maxMembers members per Space, and this would make more than that.")
  	      } else {
  	        Person.inviteMembers(rc, inviteeEmails, collabs, state).map { result =>
  	        val resultStr = 
  	          (
  	            if (result.invited.length > 0)
  	               result.invited.mkString("Sent invitations to ", ", ", ". ")
  	            else
  	               ""
  	          ) + (
  	            if (result.alreadyInvited.length > 0)
  	               result.alreadyInvited.mkString("Resent to ", ", ", ".") 
  	            else
  	               ""
  	            )
  	        resultStr
  	      }
  	    }
  	    
  	    pipe(resultFut.map(InviteResult(_))) to sender
  	  }
      
      case IsSpaceMemberP(rc) => {
        sender ! IsSpaceMember(AccessControl.isMember(rc.requesterOrAnon, state))
      }
    }
  }
}

private [spaces] object SpaceMembersActor {
  def actorProps(e:Ecology, spaceId:OID, spaceRouter:ActorRef) = Props(classOf[SpaceMembersActor], e, spaceId, spaceRouter)  
}
