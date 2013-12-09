package querki.spaces

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util._

// TODO: kill this!
import scala.concurrent.Await

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import models.{Kind, MIMEType}
import models.{AsOID, AsName, OID, ThingId, UnknownOID}
import models.{Attachment, Collection, Property, PType, PTypeBuilder, Thing, ThingState}
import messages._

import Kind._
import Thing.PropMap

import querki.identity.User

import models.system.{CollectionProp, DisplayNameProp, LinkType, NameProp, NameType, TypeProp}

import messages._
import SpaceError._

import MIMEType.MIMEType

import querki.util._
import querki.values.SpaceState

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
private [spaces] class Space(persistenceFactory:SpacePersistenceFactory) extends Actor with Requester {
  
  import context._
  import models.system.SystemSpace.{State => systemState, _} 
  
  def id = OID(self.path.name)
  
  /**
   * This is the Actor that manages all persistence (DB) operations. We do things this
   * way so that it can be stubbed out for testing.
   */
  lazy val persister = persistenceFactory.getSpacePersister(id)
  
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
  def updateState(newState:SpaceState) = {
    _currentState = Some(newState)
  }
  
  def canRead(who:User, thingId:OID):Boolean = state.canRead(who, thingId)
  def canCreate(who:User, modelId:OID):Boolean = state.canCreate(who, modelId)
  def canEdit(who:User, thingId:OID):Boolean = state.canEdit(who, thingId)
  
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
  def canChangeProperties(who:User, oldThingOpt:Option[Thing], newProps:PropMap):Unit = {
    val failedProp = changedProperties(oldThingOpt.map(_.props).getOrElse(Map.empty), newProps).find(!state.canChangePropertyValue(who, _))
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
    import models.Thing._
    import modules.Modules.Person.{identityLink, person}
    import modules.person.PersonModule._
    import querki.identity.SystemUser
    
    state.ownerIdentity.foreach { identity =>
      if (identity.localPerson(state).isEmpty) {
        createSomething(id, SystemUser, person.id, 
          toProps(
            setName(identity.handle),
            DisplayNameProp(identity.name),
            identityLink(identity.id))(),
          Kind.Thing,
          None)
      }
    }
  }
  
  def loadSpace() = {
    // TEMP: just as a proof of concept. This is entirely wrong in the long run: we should be using
    // FSM and Requester instead of blocking here:
    val persistFuture = persister ? Load
    val result = Await.result(persistFuture, scala.concurrent.duration.Duration(5, "seconds"))
    result match {
      case Loaded(state) => {
        _currentState = Some(state)
        checkOwnerIsMember()
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
      case AsName(thingName) => if (NameType.equalNames(thingName, state.name)) id else throw new Exception("Space " + state.name + " somehow got message for " + thingName)
    }
  }

  def createSomething(spaceThingId:ThingId, who:User, modelId:OID, props:PropMap, kind:Kind, attachmentInfo:Option[AttachmentInfo]) = 
  {
    val spaceId = checkSpaceId(spaceThingId)
    val name = NameProp.firstOpt(props)
    val canChangeTry = 
      TryTrans[Unit, Boolean] { canChangeProperties(who, None, props) }.
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
      persister.request(Create(state, modelId, kind, props, attachmentInfo)) {
        case Changed(thingId, modTime) => {
          kind match {
            case Kind.Thing => {
              val thing = ThingState(thingId, spaceId, modelId, () => props, modTime, kind)
              updateState(state.copy(things = state.things + (thingId -> thing)))
            }
            case Kind.Attachment => {
              val thing = new Attachment(thingId, spaceId, modelId, () => props, modTime)
              updateState(state.copy(things = state.things + (thingId -> thing)))              
            }
            case Kind.Property => {
              val typ = state.typ(TypeProp.first(props))
              val coll = state.coll(CollectionProp.first(props))
              val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
              val boundColl = coll.asInstanceOf[Collection]
              val thing = Property(thingId, spaceId, modelId, boundTyp, boundColl, () => props, modTime)
              updateState(state.copy(spaceProps = state.spaceProps + (thingId -> thing)))          
            }
            case _ => throw new Exception("Got a request to create a thing of kind " + kind + ", but don't know how yet!")
          }
        
          sender ! ThingFound(thingId, state)
        }
      }
    }    
  }
  
  def modifyThing(who:User, owner:OID, spaceThingId:ThingId, thingId:ThingId, modelIdOpt:Option[OID], pf:(Thing => PropMap)) = {
      checkSpaceId(spaceThingId)
      val oldThingOpt = state.anything(thingId)
      oldThingOpt map { oldThing =>
        val thingId = oldThing.id
        val newProps = pf(oldThing)
        val canChangeTry = 
          TryTrans[Unit, Boolean] { canChangeProperties(who, Some(oldThing), newProps) }.
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
	        
	      // TODO: this needs a clause for each Kind you can get:
          oldThing match {
            case t:Attachment => {
              persister.request(Change(state, thingId, modelId, newProps, None)) {
                case Changed(_, modTime) => {
	              val newThingState = t.copy(m = modelId, pf = () => newProps, mt = modTime)
	              updateState(state.copy(things = state.things + (thingId -> newThingState))) 
                  sender ! ThingFound(thingId, state)
                }
              }              
            }
            case t:ThingState => {
              persister.request(Change(state, thingId, modelId, newProps, None)) {
                case Changed(_, modTime) => {
	              val newThingState = t.copy(m = modelId, pf = () => newProps, mt = modTime)
	              updateState(state.copy(things = state.things + (thingId -> newThingState))) 
                  sender ! ThingFound(thingId, state)
                }
              }
            }
            case prop:Property[_,_] => {
              persister.request(Change(state, thingId, modelId, newProps, None)) {
                case Changed(_, modTime) => {
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
                }
              }
            }
            case s:SpaceState => {
              // TODO: handle changing the owner or apps of the Space. (Different messages?)
              val rawName = NameProp.first(newProps)
              val newName = NameType.canonicalize(rawName)
              val oldName = NameProp.first(oldThing.props)
              val oldDisplay = DisplayNameProp.firstOpt(oldThing.props) map (_.raw.toString) getOrElse rawName
              val newDisplay = DisplayNameProp.firstOpt(newProps) map (_.raw.toString) getOrElse rawName
              val spaceChange = if (!NameType.equalNames(newName, oldName) || !(oldDisplay.contentEquals(newDisplay))) {
                Some(SpaceChange(newName, newDisplay))
              } else {
                None
              }
              persister.request(Change(state, thingId, modelId, newProps, spaceChange)) {
                case Changed(_, modTime) => {
                  updateState(state.copy(m = modelId, pf = () => newProps, name = newName, mt = modTime))
	              sender ! ThingFound(thingId, state)
                }
              }
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
      // TODO: eventually we will allow you to delete Properties, but we're nowhere near
      // ready to cope with all the potential error conditions yet.
      if (!oldThing.isInstanceOf[ThingState] || !canEdit(who, oldThing.id)) {
        sender ! ThingError(new PublicException(ModifyNotAllowed))
      } else {
        val thingId = oldThing.id
        persister ! Delete(thingId)
        updateState(state.copy(things = state.things - thingId)) 
        sender ! ThingFound(thingId, state)
      }
    } getOrElse {
      sender ! ThingError(new PublicException(UnknownPath))
    }
  }
  
  def receive = handleResponses orElse {
    case req:CreateSpace => {
      sender ! ThingFound(UnknownOID, state)
    }

    case CreateThing(who, owner, spaceId, kind, modelId, props) => {
      createSomething(spaceId, who, modelId, props, kind, None)
    }
    
    case CreateAttachment(who, owner, spaceId, content, mime, size, modelId, props) => {
      createSomething(spaceId, who, modelId, props, Kind.Attachment, Some(AttachmentInfo(content, mime, size)))
    }
    
    case GetAttachment(who, owner, space, attachId) => {
        val attachOid = attachId match {
          case AsOID(oid) => oid
          // TODO: handle the case where this name is not recognized:
          case AsName(name) => {
            state.anythingByName(name).get.id
          }
        }
        
        // No need for further intervention -- the Persister will respond directly:
        persister.forward(LoadAttachment(attachOid))
    }
    
    case ChangeProps(who, owner, spaceThingId, thingId, changedProps) => {
      modifyThing(who, owner, spaceThingId, thingId, None, (_.props ++ changedProps))
    }
    
    case ModifyThing(who, owner, spaceThingId, thingId, modelId, newProps) => {
      modifyThing(who, owner, spaceThingId, thingId, Some(modelId), (_ => newProps))
    }
    
    case DeleteThing(who, owner, spaceThingId, thingId) => {
      deleteThing(who, spaceThingId, thingId)
    }
    
    case GetThing(req, owner, space, thingIdOpt) => {
      val thingId = thingIdOpt.flatMap(state.anything(_)).map(_.id).getOrElse(UnknownOID)
      // NOTE: Responsibility for this canRead() check has been pulled out to the Application level for now,
      // by necessity. We don't even necessarily know *who* the requester is until Application gets the
      // State back, because a locally-defined Identity requires the State.
//      if (!canRead(req, thingId))
//        sender ! ThingError(SpaceNotFound, "Space not found")
//      else 
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

object Space {
  // The name of the Space Actor
  def sid(id:OID) = id.toString
  // The OID of the Space, based on the sid
  def oid(sid:String) = OID(sid)
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(persistenceFactory:SpacePersistenceFactory):Props = Props(new Space(persistenceFactory))
}
