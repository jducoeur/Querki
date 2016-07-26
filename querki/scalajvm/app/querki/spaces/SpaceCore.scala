package querki.spaces

import scala.util.{Failure, Success}

import akka.actor._
import Actor.Receive

import org.querki.requester._

import models._
import Thing.PropMap
import Kind.Kind
import querki.core.NameUtils
import querki.globals._
import querki.identity.User
import querki.persistence._
import querki.time.DateTime
import querki.types.ModelTypeBase
import querki.types.MOIDs.ModelForTypePropOID
import querki.util.UnexpectedPublicException

import messages._
import SpaceError._
import SpaceMessagePersistence._

/**
 * This trait represents the heart of the "new style" Space Actor, based on Akka Persistence.
 * It is broken out into a trait so that we can more easily unit-test the details, separately
 * from the black-box integration testing. We're intentionally not using the standard Akka
 * TestKit, because that is explicitly unfriendly to PersistentActor.
 * 
 * This trait encapsulates the central PersistentActor concepts. It isn't actually based on
 * PersistentActor because that trait isn't very pure -- it's not obvious how to just implement
 * the trait without being an actual PersistentActor.
 */
trait SpaceCore extends SpaceMessagePersistenceBase with EcologyMember with ModelPersistence {

  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val PropTypeMigrator = interface[PropTypeMigrator]
  lazy val SpaceChangeManager = interface[SpaceChangeManager]
  
  /**
   * The OID of this Space.
   */
  def id:OID

  /**
   * Our own version of persist(). Note that this enforces UseKryo at the signature level, so we
   * should use it instead of ordinary persist().
   * 
   * This is abstract, implemented differently in the real system vs. test.
   */
  def doPersist[A <: UseKryo](event:A)(handler: (A) => Unit):Unit
  
  /**
   * The sequence ID of the most recently-processed persistence message. Normally implemented by
   * PersistentActor.
   */
  def lastSequenceNr:Long
  
  /**
   * Encapsulates "sender !" in something a bit more unit-test-friendly.
   */
  def respond(msg:AnyRef):Unit
  
  /**
   * This is where the SpaceChangeManager slots into the real process, allowing other Ecots a chance to chime
   * in on the change before it happens.
   */
  def offerChanges(who:User, modelId:Option[OID], thingOpt:Option[Thing], kind:Kind, propsIn:PropMap, changed:Seq[OID]):RequestM[ThingChangeRequest]
  
  /**
   * This was originally from SpacePersister -- it fetches a new OID to assign to a new Thing.
   */
  def allocThingId():RequestM[OID]
  
  /**
   * This states that the given state is the one to use from here on.
   */
  def updateState(newState:SpaceState, evt:Option[SpaceMessage] = None):Unit
  
  /**
   * Sends a message to the MySQL side, telling it that this Space's name has changed.
   * 
   * This code is currently in SpacePersister; we'll need to send a new message there.
   */
  def changeSpaceName(newName:String, newDisplay:String):Unit
  
  def persistMsgThen[A <: UseKryo](oid:OID, event:A, handler: => Unit):RequestM[Unit] = {
    doPersist(event) { _ =>
      handler
      respond(ThingFound(oid, state))
    }
    RequestM.successful(())
  }
  
  var _currentState:Option[SpaceState] = None
  // This should return okay any time after recovery:
  def state = {
    _currentState match {
      case Some(s) => s
      case None => throw new Exception("State not ready in Actor " + id)
    }
  }

  def canRead(who:User, thingId:OID):Boolean = AccessControl.canRead(state, who, thingId)
  def canCreate(who:User, modelId:OID):Boolean = AccessControl.canCreate(state, who, modelId)
  def canDesign(who:User, modelId:OID):Boolean = AccessControl.canDesign(state, who, modelId)
  def canEdit(who:User, thingId:OID):Boolean = AccessControl.canEdit(state, who, thingId)
  
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
  def canChangeProperties(who:User, changedProps:Seq[OID], oldThingOpt:Option[Thing], newProps:PropMap):Unit = {
    val failedProp = changedProps.find(!AccessControl.canChangePropertyValue(state, who, _))
    failedProp match {
      case Some(propId) => throw new PublicException("Space.modifyThing.propNotAllowed", state.anything(propId).get.displayName)
      case _ => oldThingOpt.map(PropTypeMigrator.checkLegalChange(state, _, newProps))
    }
  }
  
  def catchPrePersistExceptions(name:String, block: => RequestM[Unit]) = {
    block.onComplete { 
      case Success(_) => // The success case happens inside of doPersist(), which is side-effecting
      case Failure(th) => {
        th match {
          case ex:PublicException => respond(ThingError(ex))
          case ex => {
            QLog.error(s"Space.$name received an unexpected exception before doPersist()", ex)
            respond(ThingError(UnexpectedPublicException))
          }
        }
      }      
    }
  }
  
  def basedOn(props:PropMap):OID = {
    val result = for {
      basedOnVal <- props.get(ModelForTypePropOID)
      basedOn <- basedOnVal.firstAs(Core.LinkType)
    }
      yield basedOn
      
    if (result.isEmpty)
      throw new Exception("Tried to create a Type without a valid Model!")
      
    result.get
  }
  
  def doCreate(kind:Kind, thingId:OID, modelId:OID, props:PropMap, typBasedOn:Option[OID], modTime:DateTime) = {
    kind match {
      case Kind.Thing => {
        val thing = ThingState(thingId, id, modelId, props, modTime, kind)
        updateState(state.copy(things = state.things + (thingId -> thing)))
      }
      case Kind.Property => {
        val typ = state.typ(Core.TypeProp.first(props))
        val coll = state.coll(Core.CollectionProp.first(props))
        val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
        val boundColl = coll.asInstanceOf[Collection]
        val thing = Property(thingId, id, modelId, boundTyp, boundColl, props, modTime)
        updateState(state.copy(spaceProps = state.spaceProps + (thingId -> thing)))          
      }
      case Kind.Type => {
        val tpe = new ModelType(thingId, id, querki.core.MOIDs.UrTypeOID, typBasedOn.get, props)
        updateState(state.copy(types = state.types + (thingId -> tpe)))
      }
      case _ => // This shouldn't be possible -- we're checking against it in createSomething()
    }
  }

  def createSomething(who:User, modelId:OID, propsIn:PropMap, kind:Kind):RequestM[Unit] = {
    val changedProps = changedProperties(Map.empty, propsIn)
    // Let other systems put in their own oar about the PropMap:
    offerChanges(who, Some(modelId), None, kind, propsIn, changedProps).flatMap { tcr =>
      val props = tcr.newProps
      val name = Core.NameProp.firstOpt(props)
      
      // We need to sanity-check the Type Model now, before we get to persisting:
      val typBasedOn = {
        if (kind == Kind.Type) {
          Some(basedOn(props))
        } else
          None
      }
      
      // TODO: this call is side-effecting -- it throws an exception if you *aren't* allowed to change this.
      // This is stupid and sucktastic. Change to something more functional.
      canChangeProperties(who, changedProps, None, props)
      
      if (kind != Kind.Thing && kind != Kind.Property && kind != Kind.Type)
        throw new Exception("Got a request to create a thing of kind " + kind + ", but don't know how yet!")
      
      val allowed =
        if (props.contains(Core.IsModelProp.id))
          canDesign(who, modelId)
        else
          canCreate(who, modelId)
          
      if (!allowed) {
        RequestM.failed(new PublicException(CreateNotAllowed))
      } else if (name.isDefined && state.anythingByName(name.get).isDefined)
        RequestM.failed(new PublicException(NameExists, name.get))
      else {
        // All tests have passed, so now we actually persist the change: 
        val modTime = DateTime.now
        allocThingId().flatMap { thingId =>
          val msg = {
            implicit val s = state
            DHCreateThing(who, thingId, kind, modelId, props, modTime)
          }
          
          persistMsgThen(thingId, msg, doCreate(kind, thingId, modelId, props, typBasedOn, modTime))
        }
      }
    }
  }
  
  def doModify(thingId:OID, thing:Thing, modelIdOpt:Option[OID], newProps:PropMap, replaceAllProps:Boolean, modTime:DateTime) = {
    val actualProps =
      if (replaceAllProps)
        newProps
      else {
        (thing.props /: newProps) { (current, pair) =>
          val (propId, v) = pair
          if (v.isDeleted)
            // The caller has sent the special signal to delete this Property:
            current - propId
          else
            current + pair
        }
      }
    
    val modelId = modelIdOpt match {
      case Some(m) => m
      case None => thing.model
    }
    
    thing match {
      case t:ThingState => {
        val newThingState = t.copy(m = modelId, pf = actualProps, mt = modTime)
        updateState(state.copy(things = state.things + (thingId -> newThingState))) 
      }
      case prop:Property[_,_] => {
        // If the Type has changed, alter the Property itself accordingly:
        val typeChange = PropTypeMigrator.prepChange(state, prop, actualProps)
        
        val newProp = {
          if (typeChange.typeChanged)
            prop.copy(pType = typeChange.newType, m = modelId, pf = actualProps, mt = modTime)
          else
            prop.copy(m = modelId, pf = actualProps, mt = modTime)
        }
        
        updateState(state.copy(spaceProps = state.spaceProps + (thingId -> newProp)))
        
        typeChange.finish(newProp, state, updateState)            
      }
      case s:SpaceState => {
        // TODO: handle changing the owner or apps of the Space. (Different messages?)
        val newNameOpt = for {
          rawName <- Core.NameProp.firstOpt(actualProps)
          newName = NameUtils.canonicalize(rawName)
          oldName = Core.NameProp.first(thing.props)
          oldDisplay = Basic.DisplayNameProp.firstOpt(thing.props) map (_.raw.toString) getOrElse rawName
          newDisplay = Basic.DisplayNameProp.firstOpt(actualProps) map (_.raw.toString) getOrElse rawName
          if (!NameUtils.equalNames(newName, oldName) || !(oldDisplay.contentEquals(newDisplay)))
        }
          yield
        {
          changeSpaceName(newName, newDisplay)
          newName
        }
        val newName = newNameOpt.getOrElse(state.name)
        updateState(state.copy(m = modelId, pf = actualProps, name = newName, mt = modTime))
      }
      case mt:ModelTypeBase => {
        // Note that ModelTypeBase has a copy() method tuned for this purpose:
        val newType = mt.copy(modelId, actualProps)
        updateState(state.copy(types = state.types + (thingId -> newType)))
      }
    }
  }
  
  def modifyThing(who:User, thingId:ThingId, modelIdOpt:Option[OID], rawNewProps:PropMap, replaceAllProps:Boolean):RequestM[Unit] = {
    val oldThingOpt = state.anything(thingId)
    if (oldThingOpt.isEmpty)
      RequestM.failed(new PublicException(UnknownPath))
    else {
      val oldThing = oldThingOpt.get
      
      val thingId = oldThing.id
      val changedProps = changedProperties(oldThing.props, rawNewProps)
      // Let other systems put in their own oar about the PropMap:
      offerChanges(who, modelIdOpt, Some(oldThing), oldThing.kind, rawNewProps, changedProps).flatMap { tcr =>
        val newProps = tcr.newProps
        canChangeProperties(who, changedProps, Some(oldThing), newProps)
        if (!canEdit(who, thingId)) {
          RequestM.failed(new PublicException(ModifyNotAllowed))
        } else {
          val modTime = DateTime.now
          val msg = {
            implicit val s = state
            DHModifyThing(who, thingId, modelIdOpt, newProps, replaceAllProps, modTime)
          }
          
          persistMsgThen(thingId, msg, doModify(thingId, oldThing, modelIdOpt, newProps, replaceAllProps, modTime))
        }
      }
    }    
  }
  
  def doDelete(oid:OID, thing:Thing) = {
    thing match {
      case t:ThingState => updateState(state.copy(things = state.things - oid))
      case p:Property[_,_] => updateState(state.copy(spaceProps = state.spaceProps - oid))
      // This really shouldn't be possible, since deleteThing is looking it up from just the
      // things and spaceProps tables:
      case _ => throw new Exception("Somehow got a request to delete unexpected thing " + thing)
    }
  }
  
  def deleteThing(who:User, thingId:ThingId):RequestM[Unit] = {
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
    val msg = {
      implicit val s = state
      DHDeleteThing(who, oid, modTime)
    }
    persistMsgThen(oid, msg, doDelete(oid, oldThing))
  }
  
  /**
   * The standard recovery procedure for PersistentActors.
   */
  def receiveRecover:Receive = {
    case DHCreateThing(req, thingId, kind, modelId, dhProps, modTime) => {
      implicit val s = state
      val props:PropMap = dhProps 
      val typBasedOn =
        if (kind == Kind.Type)
          Some(basedOn(props))
        else
          None
          
      doCreate(kind, thingId, modelId, props, typBasedOn, modTime)
    }
    
    case DHModifyThing(req, thingId, modelIdOpt, propChanges, replaceAllProps, modTime) => {
      implicit val s = state
      val thing = state.anything(thingId).get
      val props:PropMap = propChanges
      doModify(thingId, thing, modelIdOpt, props, replaceAllProps, modTime)
    }
    
    case DHDeleteThing(req, thingId, modTime) => {
      val thing = state.anything(thingId).get
      doDelete(thingId, thing)
    }
  }
  
  /**
   * The standard PersistentActor receiveCommand, which receives and processes the messages that
   * alter the SpaceState.
   */
  def receiveCommand:Receive = {
    // This message is simple, since it isn't persisted:
    case GetSpaceInfo(who, spaceId) => {
      respond(SpaceInfo(state.id, state.name, state.displayName, state.ownerHandle))
    }
    
    case CreateThing(who, spaceId, kind, modelId, props) => {
      catchPrePersistExceptions("createSomething", createSomething(who, modelId, props, kind))
    }
    
    // TODO: remove the sync flag from ChangeProps, since it is a non-sequiteur in the Akka Persistence
    // world.
    case ChangeProps(who, spaceId, thingId, changedProps, sync) => {
      catchPrePersistExceptions("changeProps", modifyThing(who, thingId, None, changedProps, false))
    }
    
    case ModifyThing(who, spaceId, thingId, modelId, newProps) => {
      catchPrePersistExceptions("modifyThing", modifyThing(who, thingId, Some(modelId), newProps, true))
    }
    
    case DeleteThing(who, spaceId, thingId) => {
      catchPrePersistExceptions("deleteThing", deleteThing(who, thingId))
    }    
  }
}
