package querki.session

import scala.concurrent.Future
import scala.util.{Success, Failure}
import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.event.LoggingReceive
import upickle.default._
import autowire._
import org.querki.requester._
import models._
import querki.globals._
import querki.admin.SpaceTimingActor.MonitorMsg
import querki.api._
import querki.identity.{Identity, User}
import querki.history.SpaceHistory._
import querki.publication.CurrentPublicationState
import querki.session.messages._
import querki.spaces.SpaceEvolution
import querki.spaces.SpaceMessagePersistence.SpaceEvent
import querki.spaces.messages.{SpaceSubsystemRequest, ThingError, CurrentState, SpacePluginMsg, ThingFound, ChangeProps}
import querki.spaces.messages.SpaceError._
import querki.time.DateTime
import querki.uservalues.SummarizeChange
import querki.uservalues.PersistMessages._
import querki.util.{PublicException, UnexpectedPublicException, TimeoutChild, QLog}
import querki.values.{SpaceState, RequestContext, QValue}

/**
 * The Actor that controls an individual's relationship with a Space.
 * 
 * TODO: this is currently *very* incestuous with querki.uservalues. Should we refactor out the UserValue handlers,
 * along the lines of how we do in Space? That would currently leave this class very hollowed-out, but I expect it
 * to grow a lot in the future, and to become much more heterogeneous, so we may want to separate all of those
 * concerns.
 */
private [session] class OldUserSpaceSession(e:Ecology, val spaceId:OID, val user:User, val spaceRouter:ActorRef, val persister:ActorRef, timeSpaceOps:Boolean)
  extends Actor with Stash with Requester with EcologyMember with ReceivePipeline with TimeoutChild with SpaceEvolution
  with autowire.Server[String, Reader, Writer]
{
  implicit val ecology = e

  // Needed for SpacePure:
  // TODO: gah. A fine example of the problems of the inheritance-based approach. Can/should we refactor SpacePure and
  // SpaceEvolution to *actually* be pure?
  val id: OID = spaceId
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val History = interface[querki.history.History]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val Publication = interface[querki.publication.Publication]
  lazy val System = interface[querki.system.System]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  def monitor(msg: => String):Unit = {
    if (timeSpaceOps) {
      spaceRouter ! MonitorMsg(msg, DateTime.now)
    }
  }
  
//  /**
//   * IMPORTANT: this must be set before we begin any serious work! This is why we start
//   * in a rudimentary state, and don't become useful until it is received.
//   */
//  var _rawState:Option[SpaceState] = None
//  def setRawState(s:SpaceState) = {
//    clearEnhancedState()
//    _rawState = Some(s)
//  }
  var _publicationState:Option[CurrentPublicationState] = None
  def setPublicationState(s:CurrentPublicationState) = {
    clearEnhancedState()
    _publicationState = Some(s)
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
    *
    * TODO: can be deleted once we lift it out to SpaceEvolution.
   */
  def isReadException(thingId:OID)(implicit state:SpaceState):Boolean = {
    // I always have access to my own Person record, so that _me always works:
    Person.hasPerson(user, thingId)
  }

  /**
    * The state as originally received from the Space Actor, with no filtering.
    *
    * USE THIS WITH EXTREME CARE! In general, stuff should be using the enhanced state instead.
    */
  var _rawState:Option[SpaceState] = None
  /**
    * The List of events received since we last recomputed, in *reverse* order for efficiency.
    */
  var _unprocessedEvents: List[SpaceEvent] = List.empty

  /**
    * The raw State, minus stuff that this user isn't allowed to see. This mostly gets evolved with events as they come in.
    */
  var _filteredState: Option[SpaceState] = None

  /**
    * The filtered state, plus publication and userValues if appropriate. Recomputed lazily when needed.
    */
  var _enhancedState: Option[SpaceState] = None
  def clearEnhancedState() = _enhancedState = None

  def handleStateChange(stateChange: CurrentState) = {
    clearEnhancedState()
    _rawState = Some(stateChange.state)
    _unprocessedEvents = stateChange.events.getOrElse(List.empty).reverse ++ _unprocessedEvents
  }

  /**
   * The underlying SpaceState, plus all of the UserValues for this Identity.
   * 
   * This is effectively a lazy val that gets recalculated after we get a new rawState.
   */
  def state = {
    _rawState match {
      case Some(rawState) => {
        if (_enhancedState.isEmpty) {
          _filteredState = Some(updateForUser(_filteredState)(user)(rawState, _unprocessedEvents))
          _unprocessedEvents = List.empty
          val withUV = enhanceWithUserValues(_filteredState.get, userValues)
          _enhancedState = Some(enhanceWithPublication(withUV, _publicationState))
        }
        _enhancedState.get
      }
      case None => throw new Exception("UserSpaceSession trying to enhance state before there is a rawState!")
    }
  }
  
  /**
   * This is executed each time we receive a new SpaceState.
    *
    * TODO: we're not currently calling this! Deal with this idiotic edge condition in some smarter way. At the
    * least, deal with it at load time, not every time we get a new SpaceState as it previously had been doing,
    * and maybe have an additional handler when I change my name that deals with all current Spaces of mine?
    *
    * *And* this seems to be redundant with checkDisplayName() below!!!
   */
  def whenStateReady(implicit state:SpaceState) = {
    // Do I have the right Display Name? If I've changed my Identity's Display Name (which is very common
    // during the invite-accept process), update it.
    // TODO: there's a hard-to-avoid implication here, that the Person's Display Name property may be a
    // bad denormalization. I suspect we should be just going through the Identity itself.
    for {
      person <- localPerson
      personName = person.displayName
      localIdentity <- Person.localIdentities(user)
      identityId = localIdentity.id
      actualIdentity <- user.identityById(identityId)
      if (actualIdentity.name != personName)
    }
    {
      // Okay, we need to fix the name
      // TBD: note that this is being sent in the name of System. That's because I may well not have
      // Edit rights within the Space. This is a bit of a hack, but I'm not sure there is a better
      // answer -- the obvious alternative is to say that I can always change my own Person, but it
      // isn't at all clear that that's correct. In the case of, eg, whitelisted Commentators, it
      // very well might not be.
      val msg = ChangeProps(IdentityAccess.SystemUser, state.id, person.id, Map(Basic.DisplayNameProp(actualIdentity.name)))
      // TODO: we really ought to do something if this fails. But what?
      spaceRouter ? msg
    }
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

  var debugLog: List[Wikitext] = List.empty
  def addToDebugLog(wikitext: Wikitext): Unit = debugLog = (wikitext +: debugLog)
  
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
   * TODO: we're currently checking this on every SpaceSubsystemRequest, which is certainly overkill. Once we have a
   * real UserSession object, and it knows about the UserSpaceSessions, that should instead pro-actively notify
   * all of them about the change, so we don't have to hack around it.
    *
    * TODO: is this actually redundant with whenStateReady()?!? Do we have *two* different pathways doing this idiotic
    * misfeature? It does strongly show just how bad the design is.
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
   * might not in cases where the SpaceSubsystemRequest is bootstrapping this hive, or comes in while bootstrap is still
   * in process.
   * 
   * Note that this state only persists until we get a SpaceState, at which point we switch to normalReceive, and
   * stay there for the rest of this actor's life.
   */
  def receive = LoggingReceive {
    case change @ CurrentState(s, _) => {
      handleStateChange(change)
      // Now, fetch the UserValues
      // In principle, we should probably parallelize waiting for the SpaceState and UserValues:
      // the current behaviour hits first-load latency slightly. OTOH, in the interest of not hammering
      // the DB too hard, we might leave it as it is...
      persister ! LoadValuesForUser(identity, s)
    }
    
    case ps:CurrentPublicationState => {
      setPublicationState(ps)
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
    case change @ CurrentState(s, _) => handleStateChange(change)
    
    case ps:CurrentPublicationState => setPublicationState(ps)
    
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
            // First, we only allow the Managers to play with this stuff
            // TODO: we should make this a separate permission, and eventually open it more.
            // Might be available to Editors as well? But it requires global read permission.
            if (!AccessControl.isManager(user, state))
              throw new Exception(s"Only a Manager of a Space is currently allow to explore History!")
            
            // Fetch the state as of that point:
            for {
              CurrentState(state, _) <- spaceRouter.request(GetHistoryVersion(v))
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
        
    case request @ SpaceSubsystemRequest(req, space, payload) => { 
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

        case AddToDebugLog(text) => {
          addToDebugLog(text)
        }

        case GetDebugLog() => {
          sender ! CurrentDebugLog(debugLog)
        }

        case ClearDebugLog() => {
          debugLog = List.empty
          sender ! DebugLogCleared
        }
      }
    }
  }
}

object OldUserSpaceSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID, user:User, spaceRouter:ActorRef, persister:ActorRef, timeSpaceOps:Boolean):Props = 
    Props(new OldUserSpaceSession(ecology, spaceId, user, spaceRouter, persister, timeSpaceOps))
}
