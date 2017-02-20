package querki.session

import scala.concurrent.Future
import scala.util.{Failure, Success}

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.event.LoggingReceive

import upickle.default._
import autowire._

import org.querki.requester._

import models._

import querki.globals._

import querki.api._
import querki.identity.{Identity, User}
import querki.history.SpaceHistory._
import querki.session.messages._
import querki.spaces.messages.{ChangeProps, CurrentState, SessionRequest, SpacePluginMsg, ThingError, ThingFound}
import querki.spaces.messages.SpaceError._
import querki.time.DateTime
import querki.uservalues.SummarizeChange
import querki.uservalues.PersistMessages._
import querki.util.{PublicException, QLog, TimeoutChild, UnexpectedPublicException}
import querki.values.{QValue, RequestContext, SpaceState}

/**
 * The Actor that controls an individual's relationship with a Space.
 * 
 * TODO: this is currently *very* incestuous with querki.uservalues. Should we refactor out the UserValue handlers,
 * along the lines of how we do in Space? That would currently leave this class very hollowed-out, but I expect it
 * to grow a lot in the future, and to become much more heterogeneous, so we may want to separate all of those
 * concerns.
 */
private [session] class OldUserSpaceSession(e:Ecology, val spaceId:OID, val user:User, val spaceRouter:ActorRef, val persister:ActorRef)
  extends Actor with Stash with Requester with EcologyMember with ReceivePipeline with TimeoutChild
  with autowire.Server[String, Reader, Writer]
{
  implicit val ecology = e
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val History = interface[querki.history.History]
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
          val readFiltered = (rs /: rs.things) { (curState, thingPair) =>
            val (thingId, thing) = thingPair
            // Note that we need to pass rs into canRead(), not curState. That is because curState can
            // be in an inconsistent state while we're in the middle of all this. For example, we may
            // have already exised a Model from curState (because we don't have permission) when we get
            // to an Instance of that Model. Things can then get horribly confused when we try to look
            // at the Instance, try to examine its Model, and *boom*.
            if (AccessControl.canRead(rs, user, thingId) || isReadException(thingId)(rs)) {
              // Remove any SystemHidden Properties from this Thing, if there are any:
              if (hiddenOIDs.exists(thing.props.contains(_))) {
                val newThing = thing.copy(pf = { (thing.props -- hiddenOIDs) })
                curState.copy(things = (curState.things + (newThing.id -> newThing)))
              } else
                curState
            } else
              curState.copy(things = (curState.things - thingId))
          }
          
          if (AccessControl.canRead(readFiltered, user, rs.id))
            readFiltered
          else {
            // This user isn't allowed to read the Root Page, so give them a stub instead.
            // TODO: this is a fairly stupid hack, but we have to be careful -- filtering out
            // bits of the Space while not breaking it entirely is tricky. Think about how to
            // do this better.
            readFiltered.copy(pf = readFiltered.props + 
                (Basic.DisplayTextProp("**You aren't allowed to read this Page; sorry.**")))
          }
        }
        (safeState /: userValues) { (curState, uv) =>
          if (uv.thingId == curState.id) {
            // We're enhancing the Space itself:
            curState.copy(pf = (curState.props + (uv.propId -> uv.v)))
          }
          else curState.anything(uv.thingId) match {
            case Some(thing:ThingState) => {
              val newThing = thing.copy(pf = thing.props + (uv.propId -> uv.v))
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
  def checkDisplayName(req:User, space:OID) = {
    localPerson match {
      case Some(person) => {
	    val curIdentity = Person.localIdentities(req)(_rawState.get).headOption.getOrElse(user.mainIdentity)
	    val curDisplayName = curIdentity.name
	    if (person.displayName != curDisplayName) {
	      // Tell the Space to alter the name of the Person record to fit our new expectations:
	      spaceRouter ! ChangeProps(req, space, person.id, Map(Basic.DisplayNameProp(curDisplayName)))
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
  
  def changeProps(req:User, thingId:ThingId, props:PropMap) = {
    val (uvProps, nonUvProps) = props.partition { case (propId, _) => UserValues.isUserValueProp(propId)(state) }
    
    // Note that this code really isn't right! In practice, if we get more than one User Value change, the replies
    // don't line up here. But this code is, at this point, temporary, and it works better in UserValueSessionCore.
    if (!uvProps.isEmpty) {
      uvProps.foreach { case (propId, v) =>
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
                yield SpacePluginMsg(req, spaceId, SummarizeChange(thing.id, prop, summaryPropId, previous, newV))
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
    }
    
    if (!nonUvProps.isEmpty) {
      spaceRouter.forward(ChangeProps(req, spaceId, thingId, nonUvProps))
    }
  }
  
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.default.write(r)
  def read[Result: Reader](p: String) = upickle.default.read[Result](p)
  
  def mkParams(rc:RequestContext) = AutowireParams(user, Some(SpacePayload(state, spaceRouter)), rc, this, sender)
  
  def normalReceive:Receive = LoggingReceive {
    case CurrentState(s) => setRawState(s)
    
    case ClientRequest(req, rc) => {
      def handleException(th:Throwable, s:ActorRef, rc:RequestContext) = {
        th match {
          case aex:querki.api.ApiException => {
            s ! ClientError(write(aex))
          }
          case pex:PublicException => {
            QLog.error(s"Replied with PublicException $th instead of ApiException when invoking $req")
            s ! ClientError(pex.display(Some(rc)))
          }
          case ex:Exception => {
            QLog.error(s"Got exception when invoking $req", ex)
            s ! ClientError(UnexpectedPublicException.display(Some(rc)))                
          }
          case _ => {
            QLog.error(s"Got exception when invoking $req: $th")
            s ! ClientError(UnexpectedPublicException.display(Some(rc)))
          }
        }              
      }
      
      try {
        History.viewingHistoryVersion(rc) match {
          case Some(v) => {
            // We're exploring history, which is a more complex problem.
            // First, we only allow the Owner to play with this stuff:
            if (!rc.isOwner)
              throw new Exception(s"Only the Owner of a Space is currently allow to explore History!")
            
            // Fetch the state as of that point:
            for {
              CurrentState(state) <- spaceRouter.request(GetHistoryVersion(v))
            }
            {
              val params = AutowireParams(user, Some(SpacePayload(state, spaceRouter)), rc, this, sender)
              ApiInvocation.handleSessionRequest(req, params)
            }
          }
          case None => ApiInvocation.handleSessionRequest(req, mkParams(rc))
        }
      } catch {
        // Note that this only catches synchronous exceptions; asynchronous ones get
        // handled in AutowireApiImpl
        case ex:Exception => handleException(ex, sender, rc)
      }
    }
        
    case request @ SessionRequest(req, space, payload) => { 
      checkDisplayName(req, space)
      payload match {    
        case GetActiveSessions => QLog.error("OldUserSpaceSession received GetActiveSessions! WTF?")
          	    
  	    case ChangeProps2(thingId, props) => {
  	      changeProps(req, thingId, props)
  	    }
  	    
  	    case MarcoPoloRequest(propId, q, rc) => {
          val savedSender = sender
  	      new MarcoPoloImpl(mkParams(rc))(ecology).handleMarcoPoloRequest(propId, q) map { response =>
    	      savedSender ! response
          }
  	    }
      }
    }
  }
}

object OldUserSpaceSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID, user:User, spaceRouter:ActorRef, persister:ActorRef):Props = Props(new OldUserSpaceSession(ecology, spaceId, user, spaceRouter, persister))
}
