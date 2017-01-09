package querki.spaces

import scala.util.{Failure, Success, Try}

import akka.actor._
import Actor.Receive
import akka.persistence._

import models._
import models.ModelPersistence.DHSpaceState
import Thing.PropMap
import Kind.Kind
import querki.core.NameUtils
import querki.globals._
import querki.history.HistoryFunctions.SetStateReason
import querki.identity.{Identity, PublicIdentity, User}
import querki.identity.IdentityPersistence.UserRef
import querki.persistence._
import querki.time.DateTime
import querki.util.UnexpectedPublicException
import querki.values.{SpaceState, SpaceVersion}

import messages._
import SpaceError._
import SpaceMessagePersistence._
  
/**
 * The result from a single Space-changing function.
 * 
 * TODO: I believe this is strictly a Semigroup. We should probably define a Cats typeclass instance as such,
 * since we are starting to *use* it as a Semigroup.
 * 
 * @param events The event(s) -- usually one, but possibly more -- to persist this change.
 * @param changedThing The OID of the Thing (if appropriate) that this event was "about".
 * @param resultingState The State that results when this function is complete.
 */
case class ChangeResult(events:List[SpaceEvent with UseKryo], changedThing:Option[OID], resultingState:SpaceState)

/**
 * This trait represents the heart of the "new style" Space Actor, based on Akka Persistence.
 * It is broken out into a trait so that we can more easily unit-test the details, separately
 * from the black-box integration testing. We're intentionally not using the standard Akka
 * TestKit, because that is explicitly unfriendly to PersistentActor.
 * 
 * This trait encapsulates the central PersistentActor concepts. It isn't actually based on
 * PersistentActor because that trait isn't very pure -- it's not obvious how to just implement
 * the trait without being an actual PersistentActor.
 * 
 * @param rtc Effectively the abstraction of the RequestM type itself. This provides the type-level
 *    operations, and via the rm2rtc method provides instances of the abstraction. Think of those
 *    instances as being essentially instances of RequestM, and rtc as the RequestM companion object.
 */
abstract class SpaceCore[RM[_]](rtc:RTCAble[RM])(implicit val ecology:Ecology) 
  extends SpaceMessagePersistenceBase with SpaceAPI[RM] with PersistentActorCore with SpacePure with EcologyMember with ModelPersistence 
{
  /**
   * This is a bit subtle, but turns out abstract RM into a RequestTC, which has useful operations on it.
   */
  implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)

  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[querki.apps.Apps]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val SpaceChangeManager = interface[SpaceChangeManager]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemState = System.State
  
  def getSnapshotInterval = Config.getInt("querki.space.snapshotInterval", 100)
  lazy val snapshotInterval = getSnapshotInterval
  
  ////////////////////////////////////////////
  //
  // SpaceAPI
  //
  
  /**
   * This is all of the SpacePluginProvider's plugin's receives, concatenated together.
   * It is actually an important mechanism for separation of concerns. If external Ecots need
   * to inject their own messages into Space processing, they should define a SpacePlugin. That
   * will get picked up here, and added to the pipeline of processing when we receive messages.
   * 
   * The complexity here is mainly in catching exceptions. Is there no better way to do this?
   * Not only is this complex, but it *must* be the end of the call chain!
   */
  val pluginReceive:Receive =
    PartialFunction { any:Any =>
      try {
        SpaceChangeManager.spacePluginProviders.map(_.createPlugin(this, rtc).receive).reduce(_ orElse _)(any)
      } catch {
        case ex:PublicException => respond(ThingError(ex))
        case ex:Throwable => {
          QLog.error(s"Space.plugins received an unexpected exception before doPersist()", ex)
          respond(ThingError(UnexpectedPublicException))
        }
      }
    }
  
  //////////////////////////////////////////////////
  //
  // Abstract members
  //
  // These are all implemented very differently in the asynchronous, Akka Persistence-based, real Actor
  // vs. the synchronous test implementation.
  //
  
  /**
   * The OID of this Space.
   */
  def id:OID

  /**
   * This is where the SpaceChangeManager slots into the real process, allowing other Ecots a chance to chime
   * in on the change before it happens.
   */
  def offerChanges(who:User, modelId:Option[OID], thingOpt:Option[Thing], kind:Kind, propsIn:PropMap, changed:Seq[OID])(state:SpaceState):RM[ThingChangeRequest]
  
  /**
   * This was originally from SpacePersister -- it fetches a new OID to assign to a new Thing.
   */
  def allocThingId():RM[OID]
  
  /**
   * Fetches a set of new OIDs, for bulk operations.
   */
  def allocThingIds(nIds:Int):RM[Seq[OID]]
  
  /**
   * Tells any outside systems about the updated state. Originally part of Space.updateState().
   */
  def notifyUpdateState():Unit
  
  /**
   * Sends a message to the MySQL side, telling it that this Space's name has changed.
   * 
   * This code is currently in SpacePersister; we'll need to send a new message there.
   */
  def changeSpaceName(newName:String, newDisplay:String):Unit
  
  /**
   * This is called when a Space is booted up and has *no* messages in its history. In that case,
   * we should check to see if it exists in the old-style form in MySQL. 
   */
  def recoverOldSpace():RM[Option[SpaceState]]
  
  /**
   * Based on the owner's OID, go get the actual Identity. Note that we pass in the ownerId, because
   * this is typically called *before* state is set!
   */
  def fetchOwnerIdentity(ownerId:OID):RM[PublicIdentity]
  
  /**
   * Given the OID and version of an App, and the Apps that have been loaded so far, return this
   * App.
   * 
   * This is expected to be recursively dependent on loadAppsFor(), but this is the bit that actually
   * does the loading. The appsSoFar parameter is passed in solely so that it can then be passed to
   * loadAppsFor().
   * 
   * If you want to load the current version of the App, use Int.MaxValue as the version.
   */
  def loadAppVersion(appId:OID, version:SpaceVersion, appsSoFar:Map[OID, SpaceState]):RM[SpaceState]
  
  //////////////////////////////////////////////////
  
  def toPersistenceId(id:OID) = id.toThingId.toString
  def persistenceId = toPersistenceId(id)
  
  
  /**
   * This is true iff we are currently doing external async initialization *after* Recovery. While
   * that is true, we have to stash everything.
   */
  var initializing:Boolean = false
  
  
  var snapshotCounter = 0
  def doSaveSnapshot() = {
    val dhSpace = dh(currentState)
//    val dhApps = currentState.allApps.values.toSeq.map(dh(_))
    val snapshot = SpaceSnapshot(dhSpace, Seq.empty)
    saveSnapshot(snapshot)
    snapshotCounter = 0
  }
  def checkSnapshot() = {
    if (snapshotCounter > snapshotInterval)
      doSaveSnapshot()
    else
      snapshotCounter += 1
  }

  /**
   * Updates the internal state, but does *not* send out notifications. This is very occasionally correct,
   * but you should only use it when you have reason to believe that the state in question is dangerously incomplete, and
   * that another update is coming shortly.
   * 
   * This deals with updating caches, and updating the State's version number based on Akka Persistence.
   */
  def updateStateCore(newState:SpaceState, evt:Option[SpaceMessage] = None):SpaceState = {
    val filledState = 
      SpaceChangeManager.updateStateCache(CacheUpdate(evt, _currentState, newState)).
      current.
      copy(version = SpaceVersion(lastSequenceNr))
    _currentState = Some(filledState)
    filledState
  }
  
  /**
   * Look up any external cache changes, record the new state, and send notifications about it.
   */
  def updateState(newState:SpaceState, evt:Option[SpaceMessage] = None):SpaceState = {
    updateStateCore(newState, evt)
    notifyUpdateState()
    currentState
  }
  
  /**
   * A wrapper around persist() that allows us to chain from it. No clue why this isn't built into Akka Persistence.
   */
  def persistAllAnd(events:collection.immutable.Seq[UseKryo]):RM[Seq[UseKryo]] = {
    val rm = rtc.prep[Seq[UseKryo]]
    doPersistAll(events) { _ =>
      rm.resolve(Success(events))
    }
    rm
  }
  
  var _currentState:Option[SpaceState] = None
  // This should return okay any time after recovery:
  def currentState = {
    _currentState match {
      case Some(s) => s
      case None => {
        QLog.error("!!!! State not ready in Actor " + id)
        emptySpace
      }
    }
  }
  
  /**
   * The internal version of loadAppsFor -- this keeps track of the Apps loaded so far.
   * 
   * This is partly so that we don't load conflicting versions in case of diamond dependencies, but mainly
   * to prevent app dependency loops. (Which I don't think would happen in normal use, but let's
   * prevent any hackers from DDoS'ing us this way.)
   * 
   * Note that this is mutually recursive with loadAppVersion(), which actually loads a single App.
   * 
   * TODO: as currently structured, this code *probably* isn't quite right in the (probably rare) case of diamond
   * dependencies. We're currently always using the *first* specified version of an App, but I suspect
   * we should instead use the *newest* specified version. I don't think it's strictly clear, though.
   * The problem is essentially the same as that of evicted dependencies in Maven, and I suspect that,
   * as in that case, there's no obvious right answer.
   */
  def loadAppsFor(state:SpaceState, appsSoFar:Map[OID, SpaceState]):RM[SpaceState] = {
    // This does the recursive dive through the tree, returning the Apps specified in there:
    val appsRM:RM[Map[OID, SpaceState]] = (rtc.successful(appsSoFar) /: state.appInfo) { (rm, appInfo) =>
      rm.flatMap { appMap =>
        val (appId, appVersion) = appInfo
        if (appMap.contains(appId))
          rtc.successful(appMap)
        else
          loadAppVersion(appId, appVersion, appMap).map { appState =>
            appMap + (appId -> appState)
          }
      }
    }
    
    // Once that's done, set the actual Apps in this Space:
    appsRM.map { appMap =>
      // Fetch the appropriate Apps, in order, from the map:
      val spaceApps = state.appInfo.map(_._1).map(appMap(_))
      state.copy(apps = spaceApps)
    }
  }

  def canCreate(who:User, modelId:OID)(implicit state:SpaceState):Boolean = AccessControl.canCreate(state, who, modelId)
  def canDesign(who:User, modelId:OID)(implicit state:SpaceState):Boolean = AccessControl.canDesign(state, who, modelId)
  def canEdit(who:User, thingId:OID)(implicit state:SpaceState):Boolean = AccessControl.canEdit(state, who, thingId)
  
  /**
   * Goes through the Props, and figures out what is actually changing.
   */
  def changedProperties(oldProps:PropMap, newProps:PropMap):Seq[OID] = {
    val allIds = oldProps.keySet ++ newProps.keySet
    allIds.toSeq.filterNot { propId =>
      val matchesOpt = for {
        oldVal <- oldProps.get(propId)
        newVal <- newProps.get(propId)
      }
        yield oldVal.matches(newVal)
        
      matchesOpt.getOrElse(false)
    }
  }
  
  // This gets pretty convoluted, but we have to check whether the requester is actually allowed to change the specified
  // properties. (At the least, we specifically do *not* allow any Tom, Dick and Harry to change permissions!)
  // TODO: Note that this is not fully implemented in AccessControlModule yet. We'll need to flesh it out further there
  // before we generalize this feature.
  // TODO: This is horribly anti-functional -- failure is through Exceptions. Fix this when we get a chance, just on
  // general principles.
  def canChangeProperties(who:User, changedProps:Seq[OID], oldThingOpt:Option[Thing], newProps:PropMap)(implicit state:SpaceState):Unit = {
    val failedProp = changedProps.find(!AccessControl.canChangePropertyValue(state, who, _))
    failedProp map { propId => throw new PublicException("Space.modifyThing.propNotAllowed", state.anything(propId).get.displayName) }
  }
  
  def setState(who:User, newState:SpaceState, reason:SetStateReason, details:String)(state:SpaceState):RM[ChangeResult] = {
    implicit val s = state
    val evt = DHSetState(dh(newState), DateTime.now, reason.value, details)
    rtc.successful(ChangeResult(List(evt), Some(state.id), newState))
  }
  
  /**
   * Create a Person for this Space's owner.
   */
  def createOwnerPerson(identity:PublicIdentity)(state:SpaceState):RM[ChangeResult] = {
    createSomething(IdentityAccess.SystemUser, AccessControl.PersonModel.id, 
      Core.toProps(
        Core.setName(identity.handle),
        Basic.DisplayNameProp(identity.name),
        Person.IdentityLink(identity.id)),
      Kind.Thing, None)(state)
  }

  def createSomething(who:User, modelId:OID, propsIn:PropMap, kind:Kind, thingIdOpt:Option[OID])(state:SpaceState):RM[ChangeResult] = {
    implicit val s = state
    val changedProps = changedProperties(Map.empty, propsIn)
    // Let other systems put in their own oar about the PropMap:
    offerChanges(who, Some(modelId), None, kind, propsIn, changedProps)(state).flatMap { tcr =>
      val props = tcr.newProps
      val name = Core.NameProp.firstOpt(props)
      
      // We need to sanity-check the Type Model now, before we get to persisting. This will result
      // in redundant calls to basedOn() in the create path, but I think we just live with that.
      if (kind == Kind.Type) {
        basedOn(props).getOrElse(throw new Exception("Tried to create a Type without a valid Model!"))
      }
      
      // TODO: this call is side-effecting -- it throws an exception if you *aren't* allowed to change this.
      // This is stupid and sucktastic. Change to something more functional.
      canChangeProperties(who, changedProps, None, props)(state)
      
      if (kind != Kind.Thing && kind != Kind.Property && kind != Kind.Type)
        throw new Exception("Got a request to create a thing of kind " + kind + ", but don't know how yet!")
      
      val allowed =
        if (props.contains(Core.IsModelProp.id))
          canDesign(who, modelId)
        else
          canCreate(who, modelId)
          
      def isDuplicateName:Boolean = {
        val resultOpt = for {
          n <- name
          existing <- state.anythingByName(n)
        }
          // It is *not* a duplicate if it is found in an App or System; that's legal:
          yield existing.spaceId == state.id
          
        resultOpt.getOrElse(false)
      }
          
      if (!allowed) {
        rtc.failed(new PublicException(CreateNotAllowed))
      } else if (isDuplicateName)
        rtc.failed(new PublicException(NameExists, name.get))
      else {
        doCreate(who, modelId, props, kind, thingIdOpt)(state)
      }
    }
  }
  
  /**
   * The internal guts of createSomething. Note that this is exposed so that Space plugins can use it.
   */
  def doCreate(who:User, modelId:OID, props:PropMap, kind:Kind, thingIdOpt:Option[OID])(state:SpaceState):RM[ChangeResult] = {
    // All tests have passed, so now we actually persist the change: 
    val modTime = DateTime.now
    val thingIdRM = thingIdOpt.map(rtc.successful(_)).getOrElse(allocThingId())
    thingIdRM.flatMap { thingId =>
      val msg = {
        implicit val s = state
        DHCreateThing(who, thingId, kind, modelId, props, modTime, thingIdOpt.isDefined)
      }
      rtc.successful(ChangeResult(List(msg), Some(thingId), createPure(kind, thingId, modelId, props, modTime)(state)))    
    }    
  }
  
  def modifyThing(
    who:User, 
    thingId:ThingId, 
    modelIdOpt:Option[OID], 
    rawNewProps:PropMap, 
    replaceAllProps:Boolean)(state:SpaceState):RM[ChangeResult] = 
  {
    implicit val s = state
    val oldThingOpt = state.anythingLocal(thingId)
    if (oldThingOpt.isEmpty)
      rtc.failed(new PublicException(UnknownPath))
    else {
      val oldThing = oldThingOpt.get
      
      val thingId = oldThing.id
      val changedProps = changedProperties(oldThing.props, rawNewProps)
      // Let other systems put in their own oar about the PropMap:
      offerChanges(who, modelIdOpt, Some(oldThing), oldThing.kind, rawNewProps, changedProps)(state).flatMap { tcr =>
        val newProps = tcr.newProps
        canChangeProperties(who, changedProps, Some(oldThing), newProps)(state)
        if (!canEdit(who, thingId)) {
          rtc.failed(new PublicException(ModifyNotAllowed))
        } else {
          val modTime = DateTime.now
          val msg = DHModifyThing(who, thingId, modelIdOpt, newProps, replaceAllProps, modTime)
          
          rtc.successful(ChangeResult(List(msg), Some(thingId), modifyPure(thingId, oldThing, modelIdOpt, newProps, replaceAllProps, modTime)(state)))
        }
      }
    }
  }
  
  def deleteThing(who:User, thingId:ThingId)(state:SpaceState):RM[ChangeResult] = {
    implicit val s = state
    // TODO: we should probably allow deletion of local Model Types as well, but should probably check
    // that there are no Properties using that Type first.
    val oldThingOpt:Option[Thing] = 
      state.localFrom(thingId, state.things).orElse(
      state.localFrom(thingId, state.spaceProps))
    if (oldThingOpt.isEmpty)
      throw new PublicException(UnknownPath)
    val oldThing = oldThingOpt.get
    val kind = oldThing.kind
    
    // TODO: we shouldn't allow deletion of a Thing if it is being used as the basis for a Model Type
    
    if (!DataModel.isDeletable(oldThing)(state) 
        || !canEdit(who, oldThing.id))
      throw new PublicException(ModifyNotAllowed)
    
    val modTime = DateTime.now
    val oid = oldThing.id
    val msg = DHDeleteThing(who, oid, modTime)
    rtc.successful(ChangeResult(List(msg), Some(oid), deletePure(oid, oldThing)(state)))
  }
  
  /**
   * This deals with actually persisting a list of changes atomically, and produces the
   * state at the end of all that.
   * 
   * This copes with there being no actual Events to persist, but requires there be
   * at least one ChangeResult.
   */
  def persistAllThenFinalState(changes:List[ChangeResult]):RM[SpaceState] = {
    val allEvents = changes.flatMap(change => change.events)
    if (allEvents.isEmpty)
      rtc.successful(changes.last.resultingState)
    else
      persistAllAnd(allEvents).map { _ =>
        changes.last.resultingState
      }
  }
  
  /**
   * Actually execute the changes from a bunch of functions.
   * 
   * Note that this is ridiculous overkill for most cases, but is designed to work properly
   * for complex collections of operations.
   */
  def runChanges(funcs:Seq[SpaceState => RM[ChangeResult]])(state:SpaceState):RM[(List[ChangeResult], SpaceState)] = {
    // Run through all the functions, collect the results or the first error. Note
    // that the resulting list is in reverse order, simply because it's easier that way.
    val resultsRM = (rtc.successful(List.empty[ChangeResult]) /: funcs) { (rm, func) =>
      rm.flatMap { changes =>
        val stateToPass = changes.headOption.map(_.resultingState).getOrElse(state)
        func(stateToPass).map(_ +: changes)
      }
    }
    
    for {
      resultsReversed <- resultsRM
      results = resultsReversed.reverse
      persistedState <- persistAllThenFinalState(results)
      finalState = updateState(persistedState)
      _ = checkSnapshot()
    }
      yield (results, finalState)
  }
  
  /**
   * Simple wrapper around runChanges -- this runs them, and then sends the appropriate response.
   */
  def runAndSendResponse(opName:String, localCall:Boolean, func:SpaceState => RM[ChangeResult])(state:SpaceState):RM[List[ChangeResult]] = {
    val result = runChanges(Seq(func))(state)
    
    // If there was an error, respond with that.
    // TBD: the asymmetry here is awfully ugly. Does this suggest changes to RM?
    result.onComplete {
      case Success(_) =>
      case Failure(th) => {
        th match {
          case ex:PublicException => respond(ThingError(ex))
          case ex => {
            QLog.error(s"Space.$opName received an unexpected exception before doPersist()", ex)
            respond(ThingError(UnexpectedPublicException))
          }
        }
      }
    }
    
    // If it was successful, send the response for that.
    result.map { case (changes, finalState) =>
      changes.lastOption.foreach(change => change.changedThing.foreach { oid =>
        val msg = 
          if (localCall)
            ThingFound(oid, finalState)
          else
            ThingAck(oid)
        respond(msg)
      })
      changes
    }
  }
  
  /**
   * Handler for the "meta-events" that are built into Akka Persistence.
   * 
   * TBD: wow, this is horrible code. I really want a more functional version of receiveRecover, which
   * is basically folding over the incoming message stream to produce the SpaceState.
   */
  def recoverPersistence:Receive = {
    case SnapshotOffer(metadata, dh:DHSpaceState) => {
      updateStateCore(rehydrate(dh))
    }
    
    case SnapshotOffer(metadata, SpaceSnapshot(dh, dhApps)) => {
      val rawSpace = rehydrate(dh)
      val filledSpace = 
        if (dhApps.isEmpty)
          // Current code (2.2.0.1 and newer) encapsulates the Apps inside the dehydrated Space
          rawSpace
        else {
          // Old events put the Apps separately:
          val rawApps = for {
            dhApp <- dhApps
            rawApp = rehydrate(dhApp)
          }
            yield (rawApp.id -> rawApp)
          fillInApps(rawSpace, Map(rawApps:_*), Map.empty)._1
        }
      updateStateCore(filledSpace)
    }
    
    case RecoveryCompleted => {
      def readied() = {
        initializing = false
        notifyUpdateState()
        unstashAll() 
      }
      
      def readyState(originalOpt:Option[SpaceState]):RM[Unit] = {
        def afterOwnerIdentity = originalOpt match {
          case Some(original) => {
            for {
              // TODO: this should cope with the rare case where we can't find the owner's Identity. What's the
              // correct response?
              identity <- fetchOwnerIdentity(original.owner)
            }
              yield {
                val s = original.copy(ownerIdentity = Some(identity))
                // Make sure that the owner is represented by a Person object in this Space. Since this requires
                // fetching an OID, we need to loop through the standard creation pathway. Note that we can't
                // use the CreateThing message, though, since the initializing flag is blocking that pathway; we
                // have to use the call directly, and count on Requester to get around the stash.
                if (Person.localPerson(identity.id)(s).isEmpty) {
                  // This Space doesn't contains a Person for the owner yet. This generally means that we're upgrading
                  // an old MySQL Space that hasn't been touched in a long time.
                  // TODO: this code path can probably go away eventually.
                  // Note that we are only doing the internal update here, *not* sending out notifications, because
                  // we don't want to tell anybody else about it until we've added the Owner's local identity. If we
                  // send out notifications here, we can get a UserSpaceSession for the Owner in which they don't
                  // yet exist, and _hasPermission gets confused:
  //                updateStateCore(s)
                  // This will cause the notifications to go out:
                  runAndSendResponse("createOwnerPerson", true, createOwnerPerson(identity))(s)
                } else {
                  // Normal situation:
                  updateState(s)
                }
              }
          }
          
          // There's no existing Space, so we're assume this Space is newly-created. There is no actual
          // State yet -- instead, we expect to receive an InitState message once we're up and running:
          case None => rtc.successful(())
        }
        
        afterOwnerIdentity.map(_ => readied())
      }
      
      // In both of the below cases, we need to stash until we are actually ready to go. While initializing
      // is true, receiveCommand() will stash everything except Requester responses.
      if (_currentState.isEmpty) {
        // We haven't gotten *any* events, so we should go to the old-style Persister and load that way, if
        // the Space exists already.
        initializing = true
        recoverOldSpace().map { _ match {
          case Some(oldState) => {
            QLog.spew(s"Recovered old Space ${oldState.name} from MySQL; recording the BootSpace event")
            // There *is* an old Space from MySQL, so we should record that as the first event in the log:
            val msg = DHSetState(dh(oldState), DateTime.now, SetStateReason.ImportedFromMySQL.value, "")
            // Once we've recorded that, *then* we get the Space ready:
            doPersist(msg)(_ => readyState(Some(oldState)))            
          }
          
          // No old Space found, so this appears to be a brand-new Space:
          case None => readyState(None)
        }}
      } else if (currentState.ownerIdentity.isEmpty) {
        // We have a State, but we don't have an ownerIdentity, so go fetch that. This is the normal case
        // after recovery.
        initializing = true
        readyState(Some(currentState))
      } else {
        QLog.error(s"Somehow recovered Space $id with the ownerIdentity intact?")
      }
    }
  }
  
  /**
   * This looks kind of bizarre -- we're lifting the PartialFunction evolveState, then re-wrapping
   * it. Why? Because otherwise, evolveState(_currentState) gets called *once*: remember that
   * receiveRecover only gets called once, to fetch the overall processing function. So we need
   * to spell things out more explicitly.
   * 
   * IMPORTANT: this *must* be at the end of the orElse chain!
   */
  def recoverSpaceCommand:PartialFunction[Any, SpaceState] =
    PartialFunction { any =>
      any match {
        case evt:SpaceEvent => evolveState(_currentState)(evt)
        // TBD: This is horrible -- we really want to be able to filter the types without these
        // runtime matches. But AFAIK, there's no good way to do so, so we are doing the mismatch
        // checking ourselves.
        case _ => throw new Exception(s"SpaceCore.recoverSpaceCommand somehow got non-SpaceEvent $any")
      }
    }
  
  /**
   * The standard recovery procedure for PersistentActors.
   */
  def receiveRecover:Receive = recoverPersistence orElse 
    (recoverSpaceCommand andThen { newState => updateStateCore(newState) })
  
  /**
   * The standard PersistentActor receiveCommand, which receives and processes the messages that
   * alter the SpaceState.
   * 
   * This has a hardcoded switch built into it for initialization, because PersistentActor doesn't appear to
   * implement become().
   */
  def receiveCommand:Receive = {
    if (initializing) {
      // Whilst we're initializing, we need to stash everything except the responses:
      handleRequestResponse orElse {
        case _ => stash()
      }
    } else
      handleRequestResponse orElse normalReceiveCommand orElse pluginReceive
  }
  
  val normalReceiveCommand:Receive = {
    // This is the initial "set up this Space" message. It *must* be the *very first message* received
    // by this Space!
    case msg @ InitialState(who, spaceId, display, ownerId) => {
      if (_currentState.isDefined) {
        QLog.error(s"Space $id received $msg, but already has state $currentState!")
      } else {
        val msg = DHInitState(UserRef(who.id, Some(ownerId)), display)
        persistAllAnd(List(msg)).map { _ =>
          val identityOpt = who.identityById(ownerId)
          val initState = initStatePure(who.id, ownerId, identityOpt, display)
          // TODO: this shouldn't be necessary, but until everything is purified, it is:
          updateStateCore(initState)
          // Note that we intentionally do *not* use runAndSendResponse() here. That's
          // because that function responds at the end with ThingFound, which we can't
          // use for InitialState -- we're getting this call from a SpaceManager, often
          // one on a different node, and ThingFound doesn't work cross-node:
          runChanges(Seq(createOwnerPerson(identityOpt.get)))(currentState).map { _ =>
            respond(StateInitialized)
          }
        }
      }
    }
    
    case SetState(who, spaceId, newState, reason, details) => {
      val prevState = _currentState.getOrElse(emptySpace)
      runAndSendResponse("setState", true, setState(who, newState, reason, details))(prevState)
    }
    
    // This message is simple, since it isn't persisted:
    case GetSpaceInfo(who, spaceId) => {
      respond(SpaceInfo(currentState.id, currentState.name, currentState.displayName, currentState.ownerHandle))
    }
    
    case CreateThing(who, spaceId, kind, modelId, props, thingIdOpt, localCall) => {
      runAndSendResponse("createSomething", localCall, createSomething(who, modelId, props, kind, thingIdOpt))(currentState)
    }
    
    // Note that ChangeProps and ModifyThing handling are basically the same except for the replaceAllProps flag.
    // TODO: remove the sync flag from ChangeProps, since it is a non-sequiteur in the Akka Persistence
    // world.
    case ChangeProps(who, spaceId, thingId, changedProps, localCall) => {
      val initialState = currentState
      runAndSendResponse("changeProps", localCall, modifyThing(who, thingId, None, changedProps, false))(currentState).map { changes =>
        // After we finish persisting everything, we need to deal with the possibility that
        // they've changed the name of the Space...
        changes.last.changedThing.map { lastThingId => 
          if (lastThingId == spaceId) {
            // Okay, we're modifying the Space. Did we change either name?
            val finalState = changes.last.resultingState
            def linkName(state:SpaceState) = Core.NameProp.first(state.props)
            def disp(state:SpaceState) = Basic.DisplayNameProp.firstOpt(state.props) map (_.raw.toString) getOrElse linkName(state)
        
            if (!NameUtils.equalNames(linkName(finalState), linkName(initialState)) 
              || !(disp(finalState).contentEquals(disp(initialState))))
            {
              // At least one of those names changed, so notify the MySQL layer:
              changeSpaceName(NameUtils.canonicalize(linkName(finalState)), disp(finalState))
            }
          }
        }
        
      }
    }
    
    case ModifyThing(who, spaceId, thingId, modelId, newProps) => {
      runAndSendResponse("modifyThing", true, modifyThing(who, thingId, Some(modelId), newProps, true))(currentState)
    }
    
    case DeleteThing(who, spaceId, thingId) => {
      runAndSendResponse("deleteThing", true, deleteThing(who, thingId))(currentState)
    }
    
    case SaveSnapshotSuccess(metadata) => // Normal -- don't need to do anything
    case SaveSnapshotFailure(metadata, cause) => {
      // TODO: what should we do here? This explicitly isn't fatal, but it *is* scary as all heck:
      QLog.error(s"MAJOR PERSISTENCE ERROR: failed to save snapshot $metadata, because of $cause")
    }
  }
}
