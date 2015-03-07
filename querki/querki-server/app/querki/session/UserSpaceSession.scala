package querki.session

import scala.concurrent.Future
import scala.util.{Failure, Success}

import akka.actor._
import akka.event.LoggingReceive

import upickle._
import autowire._

import models.{AsName, AsOID, OID, PType, Thing, ThingId, ThingState, UnknownOID}
import models.Thing.PropMap

import querki.globals._
import Implicits.execContext

import querki.api._
import querki.conversations.{ConversationFunctions, ConversationFunctionsImpl}
import querki.identity.{Identity, User}
import querki.session.messages._
import querki.spaces.messages.{ChangeProps, CurrentState, SessionRequest, SpacePluginMsg, ThingError, ThingFound}
import querki.spaces.messages.SpaceError._
import querki.time.DateTime
import querki.uservalues.SummarizeChange
import querki.uservalues.PersistMessages._
import querki.util.{PublicException, QLog, Requester, TimeoutChild, UnexpectedPublicException}
import querki.values.{QValue, RequestContext, SpaceState}

/**
 * The Actor that controls an individual's relationship with a Space.
 * 
 * TODO: this is currently *very* incestuous with querki.uservalues. Should we refactor out the UserValue handlers,
 * along the lines of how we do in Space? That would currently leave this class very hollowed-out, but I expect it
 * to grow a lot in the future, and to become much more heterogeneous, so we may want to separate all of those
 * concerns.
 */
private [session] class UserSpaceSession(e:Ecology, val spaceId:OID, val user:User, val spaceRouter:ActorRef, val persister:ActorRef)
  extends Actor with Stash with Requester with EcologyMember with TimeoutChild
  with autowire.Server[String, upickle.Reader, upickle.Writer]
{
  implicit val ecology = e
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Person = interface[querki.identity.Person]
  lazy val System = interface[querki.system.System]
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
  
  /**
   * These are the OIDs of Properties that are marked SystemHidden -- that is, the end user should *never*
   * see them.
   * 
   * Note the implicit assumption that these are only defined in SystemState, and therefore this list is
   * immutable.
   */
  lazy val hiddenOIDs = {
    val hiddenProps = System.State.spaceProps.values.filter(_.ifSet(Basic.SystemHiddenProp)(System.State))
    hiddenProps.map(_.id)
  }
  
  /**
   * This is the dumping ground for exceptions to the rule that your Space only contains Things you can
   * read. There should *not* be many of these.
   */
  def isReadException(thingId:OID)(implicit state:SpaceState):Boolean = {
    // I always have access to my own Person record, so that _me always works:
    Person.hasPerson(user, thingId)
  }

  var _enhancedState:Option[SpaceState] = None
  def clearEnhancedState() = _enhancedState = None
  def makeEnhancedState():SpaceState = {
    _rawState match {
      case Some(rs) => {
        val isOwner = user.hasIdentity(rs.owner)
        val safeState =
    	  {
            // TODO: MAKE THIS MUCH FASTER! This is probably O(n**2), maybe worse. We need to do heavy
            // caching, and do much more sensitive updating as things change.
            (rs /: rs.things) { (curState, thingPair) =>
              val (thingId, thing) = thingPair
              // Note that we need to pass rs into canRead(), not curState. That is because curState can
              // be in an inconsistent state while we're in the middle of all this. For example, we may
              // have already exised a Model from curState (because we don't have permission) when we get
              // to an Instance of that Model. Things can then get horribly confused when we try to look
              // at the Instance, try to examine its Model, and *boom*.
              if (AccessControl.canRead(rs, user, thingId) || isReadException(thingId)(rs)) {
                // Remove any SystemHidden Properties from this Thing, if there are any:
                if (hiddenOIDs.exists(thing.props.contains(_))) {
                  val newThing = thing.copy(pf = { () => (thing.props -- hiddenOIDs) })
                  curState.copy(things = (curState.things + (newThing.id -> newThing)))
                } else
                  curState
              } else
                curState.copy(things = (curState.things - thingId))
            }
          }
        (safeState /: userValues) { (curState, uv) =>
          if (uv.thingId == curState.id) {
            // We're enhancing the Space itself:
            curState.copy(pf = () => (curState.props + (uv.propId -> uv.v)))
          }
          else curState.anything(uv.thingId) match {
            case Some(thing:ThingState) => {
              val newThing = thing.copy(pf = () => thing.props + (uv.propId -> uv.v))
              curState.copy(things = curState.things + (newThing.id -> newThing))
            }
            // Yes, this looks like an error, but it isn't: it means that there was a UserValue
            // for a deleted Thing.
            case _ => curState
          }
        }
      }
      case None => throw new Exception("UserSpaceSession trying to enhance state before there is a rawState!")
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
   * 
   * NOTE: this code runs through userValues twice, which seems redundant. That's true, but my attempt to write
   * a side-effecting version that figured out ret while doing the filtering got spoiled by the compiler being
   * *way* too clever, and apparently rearranging the order of operations to produce the wrong result. As so
   * often, side-effects in functional code are a dangerous idea.
   */
  def addUserValue(uv:OneUserValue):Option[QValue] = {
    def isMatch(oldUv:OneUserValue) = (oldUv.thingId == uv.thingId) && (oldUv.propId == uv.propId)
    
    var previous = userValues.find(isMatch(_)).map(_.v)
    userValues = userValues.filterNot(isMatch(_))
    if (!uv.v.isDeleted)
      userValues = userValues :+ uv
    clearEnhancedState()
    previous
  }
  
  val timeoutConfig = "querki.session.timeout"
    
  /**
   * The Identity that we are using in this Space.
   * 
   * TODO: this isn't quite right. This UserSpaceSession really ought to be for the Identity in the first place,
   * but we don't have enough of the Identity infrastructure at all levels -- in particular, the messages are
   * currently communicating User where they should be communicating Identity. So for now, we're calculating
   * the best option here, which is the local Identity if one is available, and the primary Identity if not.
   */
  var _identity:Option[Identity] = None
  def identity = {
    _identity match {
      case Some(ident) => ident
      case None => {
        _identity = Some(Person.localIdentities(user)(_rawState.get).headOption.getOrElse(user.mainIdentity))
        _identity.get
      }
    }
  }
  
  def localPerson = Person.localPerson(identity)(_rawState.get)
  
  /**
   * This is here in order to check that the user's Display Name hasn't changed.
   * 
   * TODO: we're currently checking this on every SessionRequest, which is certainly overkill. Once we have a
   * real UserSession object, and it knows about the UserSpaceSessions, that should instead pro-actively notify
   * all of them about the change, so we don't have to hack around it.
   * 
   * TBD: in general, the way we have denormalized the Display Name between Identity and Person is kind of suspicious.
   * There are good efficiency arguments for it, but I am suspicious.
   */
  def checkDisplayName(req:User, own:OID, space:ThingId) = {
    localPerson match {
      case Some(person) => {
	    val curIdentity = Person.localIdentities(req)(_rawState.get).headOption.getOrElse(user.mainIdentity)
	    val curDisplayName = curIdentity.name
	    if (person.displayName != curDisplayName) {
	      // Tell the Space to alter the name of the Person record to fit our new expectations:
	      spaceRouter ! ChangeProps(req, own, space, person.id, Map(Basic.DisplayNameProp(curDisplayName)))
	      // Update the local identity:
	      _identity = Some(curIdentity)
	    }
      }
      case None => {}
    }
  }

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
      persister ! LoadValuesForUser(identity, s)
    }
    
    case ValuesForUser(uvs) => {
      clearEnhancedState()
      userValues = uvs
      // Okay, ready to roll:
      unstashAll()
      context.become(normalReceive)      
    }
    
    case _ => stash()
  }
  
  def changeProps(currentRequest:SessionRequest, thingId:ThingId, props:PropMap) = {
    val SessionRequest(req, own, space, payload) = currentRequest
    
    // For the time being, we cope only with a single UserValue property being set at a time.
    // TODO: generalize this properly!
    val uvPropPairOpt:Option[(OID, QValue)] = 
      if (props.size == 1) {
        val (propId, v) = props.head
        if (UserValues.isUserValueProp(propId)(state))
          Some((propId, v))
        else
          None
      } else
        None
          
    uvPropPairOpt match {
      // It's a UserValue, so persist it that way:
      case Some((propId, v)) => {
        state.anything(thingId) match {
          case Some(thing) => {
            if (AccessControl.hasPermission(UserValues.UserValuePermission, state, identity.id, thing.id)) {
              implicit val s = state
              // Persist the change...
              val uv = OneUserValue(identity, thing.id, propId, v, DateTime.now)
              val previous = addUserValue(uv)
 	          persister ! SaveUserValue(uv, state, previous.isDefined)
   	          
              // ... then tell the Space to summarize it, if there is a Summary Property...
   	          val msg = for {
   	            prop <- state.prop(propId) orElse QLog.warn(s"UserSpaceSession.ChangeProps2 got unknown Property $propId")
   	            summaryLinkPV <- prop.getPropOpt(UserValues.SummaryLink)
   	            summaryPropId <- summaryLinkPV.firstOpt
   	            newV = if (v.isDeleted) None else Some(v)
   	          }
                yield SpacePluginMsg(req, own, space, SummarizeChange(thing.id, prop, summaryPropId, previous, newV))
              msg.map(spaceRouter ! _)
                
              // ... then tell the user we're set.
              sender ! ThingFound(thing.id, state)
            } else {
              // Should we log a warning here? It *is* possible to get here, if the permission changed between the
              // time the user loaded the page and the time they submitted it.
              sender ! ThingError(new PublicException(ModifyNotAllowed))
            }
          }
          case None => sender ! ThingError(UnexpectedPublicException, Some(state))
        }
      }
      // It's not a UserValue, so just tell the Space about the change:
      case None => spaceRouter.forward(ChangeProps(req, own, space, thingId, props))
    }    
  }
  
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.write(r)
  def read[Result: Reader](p: String) = upickle.read[Result](p)
  
  def mkParams(rc:RequestContext) = AutowireParams(user, state, rc + state, spaceRouter, this)
  
  def normalReceive:Receive = LoggingReceive {
    case CurrentState(s) => setRawState(s)
    
    case request @ SessionRequest(req, own, space, payload) => { 
      checkDisplayName(req, own, space)
      payload match {
        
        case ClientRequest(req, rc) => {
          val apiName = req.path(2)
            
          def handleException(ex:Throwable, s:ActorRef, rc:RequestContext) = {
            ex match {
              case aex:querki.api.ApiException => s ! ClientError(write(aex))
              case pex:PublicException => {
                QLog.error(s"$apiName replied with PublicException $ex instead of ApiException when invoking $req")
                s ! ClientError(pex.display(Some(rc)))
              }
              case _ => {
                QLog.error(s"Got exception from $apiName when invoking $req: $ex")
                s ! UnexpectedPublicException.display(Some(rc))
              }
            }              
          }
          
          try {
            def params = mkParams(rc)
            
            def handleRequest[T <: AutowireApiImpl](mkHandler: => T)(doRoute:T => Future[String]) = {
                // route() is asynchronous, so we need to store away the sender!
                val senderSaved = sender
                val handler = mkHandler
                doRoute(handler).onComplete { 
                  case Success(result) => senderSaved ! ClientResponse(result)
                  case Failure(ex) => handleException(ex, senderSaved, handler.rc)
                }              
            }
          
            apiName match {
              case "ThingFunctions" => {
                handleRequest(new ThingFunctionsImpl(params))(route[ThingFunctions](_)(req))
              }
              case "EditFunctions" => {
                handleRequest(new EditFunctionsImpl(params))(route[EditFunctions](_)(req))
              }
              case "SearchFunctions" => {
                handleRequest(new SearchFunctionsImpl(params))(route[SearchFunctions](_)(req))
              }
              case "ConversationFunctions" => {
                handleRequest(new ConversationFunctionsImpl(params))(route[ConversationFunctions](_)(req))
              }
              case "SecurityFunctions" => {
                handleRequest(new querki.security.SecurityFunctionsImpl(params))(route[SecurityFunctions](_)(req))
              }
              case _ => { sender ! ClientError("Unknown API ID!") }
            }
          } catch {
            case ex:Exception => handleException(ex, sender, rc)
          }
        }
        
        case GetActiveSessions => QLog.error("UserSpaceSession received GetActiveSessions! WTF?")
        
	    case GetThing(thingIdOpt) => {
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
	      changeProps(request, thingId, props)
	    }
	    
	    case MarcoPoloRequest(propId, q, rc) => {
	      val response = new MarcoPoloImpl(mkParams(rc))(ecology).handleMarcoPoloRequest(propId, q)
	      sender ! response
	    }
      }
    }
  }
}

object UserSpaceSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID, user:User, spaceRouter:ActorRef, persister:ActorRef):Props = Props(new UserSpaceSession(ecology, spaceId, user, spaceRouter, persister))
}
