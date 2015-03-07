package querki.spaces

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util._

// TODO: kill this!
import scala.concurrent.Await

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout

import models.{Kind, MIMEType}
import models.{AsOID, AsName, OID, ThingId, UnknownOID}
import models.{Collection, Property, PType, PTypeBuilder, Thing, ThingState}
import models.Thing.emptyProps
import MIMEType.MIMEType
import Kind._
import Thing.PropMap

import querki.core.NameUtils
import querki.ecology._
import querki.identity.User
import querki.time.DateTime
import querki.types.ModelTypeDefiner
import querki.types.MOIDs.ModelForTypePropOID
import querki.util._
import querki.values.SpaceState

import messages._
import SpaceError._
import PersistMessages._

/**
 * The Actor that encapsulates a Space.
 * 
 * As a hard and fast rule, application code in Querki never alters a Space directly.
 * Instead, all changes are described as messages to the Space's Actor; that is
 * responsible for making the actual changes. This way, the database layer is firmly
 * encapsulated, and race conditions are prevented.
 * 
 * Similarly, you fetch the Space's current State from the Space Actor. The State is
 * an immutable object completely describing the Space at a single moment; you are
 * allowed to process that in any way, just not change it.
 * 
 * An Actor's name is based on its OID, as is its Thing Table in the DB. Use Space.sid()
 * to get the name. Note that this has *nothing* to do with the Space's Display Name, which
 * is user-defined. (And unique only to that user.)
 */
private [spaces] class Space(val ecology:Ecology, persistenceFactory:SpacePersistenceFactory, stateRouter:ActorRef, id:OID) 
  extends Actor with Requester with EcologyMember with ModelTypeDefiner with SpaceAPI
{
  
  import context._
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Person = interface[querki.identity.Person]
  lazy val Core = interface[querki.core.Core]
  lazy val PropTypeMigrator = interface[PropTypeMigrator]
  lazy val SpaceChangeManager = interface[SpaceChangeManager]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val Conversations = interface[querki.conversations.Conversations]
  
  /**
   * This is the Actor that manages all persistence (DB) operations. We do things this
   * way so that it can be stubbed out for testing.
   */
  lazy val persister = persistenceFactory.getSpacePersister(id)
  
  /**
   * Our requests are going mainly to the Persister, which is talking to the DB, so give them
   * lots of time.
   * 
   * TODO: this should probably become config-driven, so testing can clamp it down?
   */
  override implicit val requestTimeout = Timeout(DurationInt(30) seconds)
  
  /**
   * This is all of the SpacePluginProvider's plugin's receives, concatenated together.
   * It is actually an important mechanism for separation of concerns. If external Ecots need
   * to inject their own messages into Space processing, they should define a SpacePlugin. That
   * will get picked up here, and added to the pipeline of processing when we receive messages.
   */
  val pluginReceive = SpaceChangeManager.spacePluginProviders.map(_.createPlugin(this).receive).reduce(_ orElse _)
  
  /**
   * TODO: now that I understand Akka better, this is probably better reimplemented as a
   * local parameter of the Receive function. That is, we should start in a Loading
   * state, stashing incoming messages to begin with, and do each DB load (from Spaces
   * and then from the Space table) as its own message, with a DBLoader child Actor
   * driving the load process. Only once we have constructed the SpaceState do we
   * become(ready(state)), and each mutation does a become(ready(newState)). That
   * eliminates this var, and is more true to Akka style. (It also provides us with
   * a clearer approach for dealing with sending out "Loading" indications.) 
   */
  var _currentState:Option[SpaceState] = None
  // This should return okay any time after preStart:
  def state = {
    _currentState match {
      case Some(s) => s
      case None => throw new Exception("State not ready in Actor " + id)
    }
  }
  // TODO: keep the previous states here in the Space, so we can undo/history
  /**
   * Change to a new State.
   * 
   * This sends notifications to all of the Cache listeners, allowing them to update their particular
   * cache entries. They may use the evt to optimize their updates, so the message should be included
   * here when possible.
   */
  def updateState(newState:SpaceState, evt:Option[SpaceMessage] = None) = {
    val withCaches = SpaceChangeManager.updateStateCache(CacheUpdate(evt, _currentState, newState))
    _currentState = Some(withCaches.current)
    stateRouter ! CurrentState(_currentState.get)
  }
  
  def canRead(who:User, thingId:OID):Boolean = AccessControl.canRead(state, who, thingId)
  def canCreate(who:User, modelId:OID):Boolean = AccessControl.canCreate(state, who, modelId)
  def canEdit(who:User, thingId:OID):Boolean = AccessControl.canEdit(state, who, thingId)
  
  def changedProperties(oldProps:PropMap, newProps:PropMap):Seq[OID] = {
    val allIds = oldProps.keySet ++ newProps.keySet
    allIds.toSeq.filterNot { propId =>
      val matchesOpt = for (
        oldVal <- oldProps.get(propId);
        newVal <- newProps.get(propId)
          )
        yield oldVal.matches(newVal)
        
      matchesOpt.getOrElse(false)
    }
  }
  
  // This gets pretty convoluted, but we have to check whether the requester is actually allowed to change the specified
  // properties. (At the least, we specifically do *not* allow any Tom, Dick and Harry to change permissions!)
  // TODO: Note that this is not fully implemented in AccessControlModule yet. We'll need to flesh it out further there
  // before we generalize this feature.
  def canChangeProperties(who:User, changedProps:Seq[OID], oldThingOpt:Option[Thing], newProps:PropMap):Unit = {
    val failedProp = changedProps.find(!AccessControl.canChangePropertyValue(state, who, _))
    failedProp match {
      case Some(propId) => throw new PublicException("Space.modifyThing.propNotAllowed", state.anything(propId).get.displayName)
      case _ => oldThingOpt.map(PropTypeMigrator.checkLegalChange(state, _, newProps))
    }
  }
  
  /**
   * When we load the Space, make sure that the Owner has a matching Person record, so they show up
   * as a Member. This will mainly affect newly-created Spaces.
   * 
   * TBD: all these internal imports are a bad smell. This probably belongs elsewhere, but where?
   */
  def checkOwnerIsMember() = {
    import querki.identity.SystemUser
    
    state.ownerIdentity.foreach { identity =>
      if (Person.localPerson(identity)(state).isEmpty) {
        createSomething(id, SystemUser, AccessControl.PersonModel.id, 
          Core.toProps(
            Core.setName(identity.handle),
            Basic.DisplayNameProp(identity.name),
            Person.IdentityLink(identity.id))(),
          Kind.Thing)
      }
    }
  }
  
  def loadSpace() = {
    // TEMP: temporary logging message, to see if *anything* is coming out:
    println("TEMP: Loading Space " + id)
    // TEMP: just as a proof of concept. This is entirely wrong in the long run: we should be using
    // FSM and Requester instead of blocking here:
    val persistFuture = persister ? Load
    val result = Await.result(persistFuture, scala.concurrent.duration.Duration(5, "seconds"))
    result match {
      case Loaded(s) => {
        updateState(s)
        checkOwnerIsMember()
        // Okay, we're up and running. Tell any listeners about the current state:
        stateRouter ! CurrentState(state)
      }
      case _ => QLog.error("Got an error!")
    }
  }
  
  override def preStart() = {
    loadSpace()
  } 
  
  def checkSpaceId(thingId:ThingId):OID = {
    thingId match {
      case AsOID(oid) => if (oid == id) oid else throw new Exception("Space " + id + " somehow got message for " + oid)
      case AsName(thingName) => if (NameUtils.equalNames(thingName, state.name)) id else throw new Exception("Space " + state.name + " somehow got message for " + thingName)
    }
  }

  def createSomething(spaceThingId:ThingId, who:User, modelId:OID, propsIn:PropMap, kind:Kind) = 
  {
    val spaceId = checkSpaceId(spaceThingId)
    val changedProps = changedProperties(Map.empty, propsIn)
    // Give listeners a chance to make alterations
    val props = SpaceChangeManager.thingChanges(ThingChangeRequest(state, Some(modelId), None, propsIn, changedProps)).newProps
    val name = Core.NameProp.firstOpt(props)
    val canChangeTry = 
      TryTrans[Unit, Boolean] { canChangeProperties(who, changedProps, None, props) }.
        onSucc { _ => true }.
        onFail { ex => sender ! ThingError(ex); false }.
        result
    if (!canCreate(who, modelId))
      sender ! ThingError(new PublicException(CreateNotAllowed))
    else if (!canChangeTry) {
      // We've already sent the error at this point
    } else if (name.isDefined && state.anythingByName(name.get).isDefined)
      sender ! ThingError(new PublicException(NameExists, name.get))
    else {
      val modTime = DateTime.now
      persister.request(Create(state, modelId, kind, props, modTime)) {
        case Changed(thingId, _) => {
          implicit val e = ecology
          kind match {
            case Kind.Thing => {
              val thing = ThingState(thingId, spaceId, modelId, () => props, modTime, kind)
              updateState(state.copy(things = state.things + (thingId -> thing)))
            }
            case Kind.Property => {
              val typ = state.typ(Core.TypeProp.first(props))
              val coll = state.coll(Core.CollectionProp.first(props))
              val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
              val boundColl = coll.asInstanceOf[Collection]
              val thing = Property(thingId, spaceId, modelId, boundTyp, boundColl, () => props, modTime)
              updateState(state.copy(spaceProps = state.spaceProps + (thingId -> thing)))          
            }
            case Kind.Type => {
              val typOpt = for (
                basedOnVal <- props.get(ModelForTypePropOID);
                basedOn <- basedOnVal.firstAs(Core.LinkType)
                  )
                yield new ModelType(thingId, basedOn, () => props)
              
              typOpt match {
                case Some(typ) => updateState(state.copy(types = state.types + (thingId -> typ)))
                case None => throw new Exception("Tried to create a Type without a valid Model!")
              }
            }
            case _ => throw new Exception("Got a request to create a thing of kind " + kind + ", but don't know how yet!")
          }
        
          sender ! ThingFound(thingId, state)
        }
      }
    }    
  }
  
  def modifyThing(who:User, thingId:ThingId, modelIdOpt:Option[OID], pf:(Thing => PropMap)) = {
      val oldThingOpt = state.anything(thingId)
      oldThingOpt map { oldThing =>
        val thingId = oldThing.id
        val rawNewProps = pf(oldThing)
        val changedProps = changedProperties(oldThing.props, rawNewProps)
        val newProps = SpaceChangeManager.thingChanges(ThingChangeRequest(state, modelIdOpt, Some(oldThing), rawNewProps, changedProps)).newProps
        val canChangeTry = 
          TryTrans[Unit, Boolean] { canChangeProperties(who, changedProps, Some(oldThing), newProps) }.
            onSucc { _ => true }.
            onFail { ex => sender ! ThingError(ex); false }.
            result
        if (!canEdit(who, thingId)) {
          sender ! ThingError(new PublicException(ModifyNotAllowed))
        } else if (!canChangeTry) {
          // Error already sent
        } else {
          // TODO: compare properties, build a history record of the changes
	      val modelId = modelIdOpt match {
	        case Some(m) => m
	        case None => oldThing.model
	      }
	        
          implicit val e = ecology
          val modTime = DateTime.now
          // Make the change in-memory...
	      // TODO: this needs a clause for each Kind you can get:
          val spaceChangeOpt = oldThing match {
            case t:ThingState => {
              val newThingState = t.copy(m = modelId, pf = () => newProps, mt = modTime)
              updateState(state.copy(things = state.things + (thingId -> newThingState))) 
              sender ! ThingFound(thingId, state)
              None
            }
            case prop:Property[_,_] => {
              // If the Type has changed, alter the Property itself accordingly:
              val typeChange = PropTypeMigrator.prepChange(state, prop, newProps)
              
              val newProp = {
                if (typeChange.typeChanged)
                  prop.copy(pType = typeChange.newType, m = modelId, pf = () => newProps, mt = modTime)
                else
                  prop.copy(m = modelId, pf = () => newProps, mt = modTime)
              }
              
              updateState(state.copy(spaceProps = state.spaceProps + (thingId -> newProp)))
              
              typeChange.finish(newProp, state, updateState)
              
              sender ! ThingFound(thingId, state)
              
              None
            }
            case s:SpaceState => {
              // TODO: handle changing the owner or apps of the Space. (Different messages?)
              val rawName = Core.NameProp.first(newProps)
              val newName = NameUtils.canonicalize(rawName)
              val oldName = Core.NameProp.first(oldThing.props)
              val oldDisplay = Basic.DisplayNameProp.firstOpt(oldThing.props) map (_.raw.toString) getOrElse rawName
              val newDisplay = Basic.DisplayNameProp.firstOpt(newProps) map (_.raw.toString) getOrElse rawName
              val spaceChange = if (!NameUtils.equalNames(newName, oldName) || !(oldDisplay.contentEquals(newDisplay))) {
                Some(SpaceChange(newName, newDisplay))
              } else {
                None
              }
              
              updateState(state.copy(m = modelId, pf = () => newProps, name = newName, mt = modTime))
              sender ! ThingFound(thingId, state)
              
              spaceChange
            }
	      }
          
          // ... and persist the change. Note that this is fire-and-forget, and happens after we respond to the caller!
          persister.request(Change(state, thingId, modelId, modTime, newProps, spaceChangeOpt)) {
            case Changed(_, _) => {
              // Do we do anything? This just signifies success
            }
            case err:Any => {
              // TODO: this is one of those relatively rare situations where we potentially want to
              // raise Big Red Flags -- serialization failure is Bad.
              QLog.error(s"Attempt to serialize $thingId in space ${state.id} failed!!! New props were $newProps, and returned value was $err")
            }
          }
	    }
      } getOrElse {
        sender ! ThingError(new PublicException(UnknownPath))
      }    
  }
  
  def deleteThing(who:User, spaceThingId:ThingId, thingId:ThingId) = {
    val spaceId = checkSpaceId(spaceThingId)
    val oldThingOpt = state.anything(thingId)
    oldThingOpt map { oldThing =>
      if (!DataModel.isDeletable(oldThing)(state) || !canEdit(who, oldThing.id)) {
        sender ! ThingError(new PublicException(ModifyNotAllowed))
      } else {
        val thingId = oldThing.id
        persister ! Delete(thingId)
        oldThing match {
          case t:ThingState => updateState(state.copy(things = state.things - thingId))
          case p:Property[_,_] => updateState(state.copy(spaceProps = state.spaceProps - thingId))
          case _ => throw new Exception("Somehow got a request to delete unexpected thing " + oldThing)
        }
        sender ! ThingFound(thingId, state)
      }
    } getOrElse {
      sender ! ThingError(new PublicException(UnknownPath))
    }
  }
  
  // If it isn't a message that we know how to handle, let the plugins take a crack at it:
  def receive = LoggingReceive(mainReceive orElse pluginReceive)
  
  def mainReceive:Receive = {
    case req:CreateSpace => {
      sender ! ThingFound(UnknownOID, state)
    }

    case CreateThing(who, owner, spaceId, kind, modelId, props) => {
      createSomething(spaceId, who, modelId, props, kind)
    }
    
    case ChangeProps(who, owner, spaceThingId, thingId, changedProps) => {
      modifyThing(who, thingId, None, { thing =>
        (thing.props /: changedProps) { (current, pair) =>
          val (propId, v) = pair
          if (v.isDeleted)
            // The caller has sent the special signal to delete this Property:
            current - propId
          else
            current + pair
        }
      })
    }
    
    case ModifyThing(who, owner, spaceThingId, thingId, modelId, newProps) => {
      modifyThing(who, thingId, Some(modelId), (_ => newProps))
    }
    
    case DeleteThing(who, owner, spaceThingId, thingId) => {
      deleteThing(who, spaceThingId, thingId)
    }
  }
}

object Space {
  // The OID of the Space, based on the sid
  def oid(sid:String) = OID(sid)
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, persistenceFactory:SpacePersistenceFactory, stateRouter:ActorRef, id:OID):Props = Props(new Space(ecology, persistenceFactory, stateRouter, id))
}
