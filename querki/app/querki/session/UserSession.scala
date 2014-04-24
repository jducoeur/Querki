package querki.session

import akka.actor._
import akka.event.LoggingReceive

import models.{AsName, AsOID, OID, PType, ThingState, UnknownOID}

import querki.ecology._
import querki.identity.{Identity, User}
import querki.session.messages._
import querki.spaces.messages.{ChangeProps, CurrentState, SessionRequest, ThingError, ThingFound}
import querki.spaces.messages.SpaceError._
import querki.time.DateTime
import querki.uservalues.UserValueWrapper
import querki.util.{PublicException, QLog, TimeoutChild, UnexpectedPublicException}
import querki.values.{QValue, SpaceState}

import querki.uservalues.PersistMessages._

private [session] class UserSession(val ecology:Ecology, val spaceId:OID, val user:User, val spaceRouter:ActorRef, val persister:ActorRef)
  extends Actor with Stash with EcologyMember with TimeoutChild
{
  lazy val Person = interface[querki.identity.Person]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  /**
   * IMPORTANT: this must be set before we begin any serious work! This is why we start
   * in a rudimentary state, and don't become useful until it is received.
   */
  var _rawState:Option[SpaceState] = None
  def setRawState(s:SpaceState) = {
    clearEnhancedState()
    _rawState = Some(s)
  }

  var _enhancedState:Option[SpaceState] = None
  def clearEnhancedState() = _enhancedState = None
  def makeEnhancedState():SpaceState = {
    implicit val e = ecology
    _rawState match {
      case Some(rs) => {
        (rs /: userValues) { (curState, uv) =>
          curState.anything(uv.thingId) match {
            case Some(thing:ThingState) => {
              val prop = curState.prop(uv.propId).getOrElse(throw new Exception("Trying to enhance with unknown Property " + uv.propId))
              val wrapper = UserValues.wrapUserValue(uv.v, prop.pType, thing.localProp(prop).map(_.v))
              val newThing = thing.copy(pf = () => thing.props + (uv.propId -> wrapper))
              curState.copy(things = curState.things + (newThing.id -> newThing))
            }
            // Yes, this looks like an error, but it isn't: it means that there was a UserValue
            // for a deleted Thing.
            case _ => curState
          }
        }
      }
      case None => throw new Exception("UserSession trying to enhance state before there is a rawState!")
    }
  }
  /**
   * The underlying SpaceState, plus all of the UserValues for this Identity.
   * 
   * This is effectively a lazy val that gets recalculated after we get a new rawState.
   */
  def state = {
    if (_enhancedState.isEmpty)
      _enhancedState = Some(makeEnhancedState())
    _enhancedState.get
  }
  
  /**
   * This Identity's distinct values in this Space.
   * 
   * TBD: should we switch this to a Map? We do need to update it when new values are given. But the
   * key would be complex -- thingId+propId. Not sure whether it's worth it or not.
   */
  var userValues = Seq.empty[OneUserValue]
  
  /**
   * Add the given UserValue to the known ones. If there was previously a value, overwrite it and return true.
   */
  def addUserValue(uv:OneUserValue):Boolean = {
    var ret = false
    userValues = 
      userValues.
        filterNot { oldUv => 
          val isMatch = (oldUv.thingId == uv.thingId) && (oldUv.propId == uv.propId)
          if (isMatch) ret = true
          isMatch
        } :+ uv
    clearEnhancedState()
    ret
  }
  
  val timeoutConfig = "querki.session.timeout"
    
  /**
   * The Identity that we are using in this Space.
   * 
   * TODO: this isn't quite right. This UserSession really ought to be for the Identity in the first place,
   * but we don't have enough of the Identity infrastructure at all levels -- in particular, the messages are
   * currently communicating User where they should be communicating Identity. So for now, we're calculating
   * the best option here, which is the local Identity if one is available, and the primary Identity if not.
   */
  lazy val identity = Person.localIdentities(user)(_rawState.get).headOption.getOrElse(user.mainIdentity) 

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
      setRawState(s)
      // Now, fetch the UserValues
      // In principle, we should probably parallelize waiting for the SpaceState and UserValues:
      // the current behaviour hits first-load latency slightly. OTOH, in the interest of not hammering
      // the DB too hard, we might leave it as it is...
      persister ! LoadValuesForUser(identity.id, s)
    }
    
    case ValuesForUser(identityId, uvs) => {
      clearEnhancedState()
      userValues = uvs
      // Okay, ready to roll:
      unstashAll()
      context.become(normalReceive)      
    }
    
    case _ => stash()
  }
  
  def normalReceive:Receive = LoggingReceive {
    case CurrentState(s) => setRawState(s)
    
    case SessionRequest(req, own, space, payload) => { 
      payload match {
        case GetActiveSessions => QLog.error("UserSession received GetActiveSessions! WTF?")
        
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
	    
	    case ChangeProps2(thingId, props) => {
	      // For the time being, we cope only with a single UserValue property being set at a time.
	      // TODO: generalize this properly!
	      val uvPropPairOpt:Option[(PType[_], OID, QValue)] = 
	        if (props.size == 1) {
	          val (propId, v) = props.head
	          state.prop(propId).flatMap{ prop =>
	            UserValues.getUserType(prop.pType).map((_, propId, v))
	          }
	        } else
	          None
	          
	      uvPropPairOpt match {
	        // It's a UserValue, so persist it that way:
	        case Some((uvt, propId, v)) => {
	          state.anything(thingId) match {
	            case Some(thing) => {
	              val uv = OneUserValue(thing.id, propId, v, DateTime.now)
	              val existed = addUserValue(uv)
       	          persister ! SaveUserValue(identity.id, uv, state, existed)
	              // TODO: ask the Space to update the summary!
	              sender ! ThingFound(thing.id, state)
	            }
	            case None => sender ! ThingError(UnexpectedPublicException, Some(state))
	          }
	        }
	        // It's not a UserValue, so just tell the Space about the change:
	        case None => spaceRouter.forward(ChangeProps(req, own, space, thingId, props))
	      }
	    }     
      }
    }
  }
}

object UserSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID, user:User, spaceRouter:ActorRef, persister:ActorRef):Props = Props(new UserSession(ecology, spaceId, user, spaceRouter, persister))
}
