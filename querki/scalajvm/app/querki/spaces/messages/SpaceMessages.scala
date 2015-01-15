package querki.spaces.messages

import language.implicitConversions

import models.{Kind, MIMEType, Thing}
import Kind._
import MIMEType.MIMEType
import models.Thing.PropMap
import models.{AsOID, OID, ThingId, UnknownOID}

import querki.conversations.messages.ConversationMessage
import querki.identity.User
import querki.session.messages.SessionMessage
import querki.values.{RequestContext, SpaceState}
import querki.util.PublicException

sealed trait SpaceMgrMsg

// TBD: this arguably doesn't belong to SpaceManager. There is no serious reason why it needs to
// go through SpaceManager, since it doesn't get routed to a Space, and so efficiency, if nothing
// else, argues for having something else resposible for it. Indeed, it isn't even strictly
// clear that it needs to go through a separate Actor at all -- we *could* deal with this
// at the tip.
case class ListMySpaces(owner:OID) extends SpaceMgrMsg
sealed trait ListMySpacesResponse
case class SpaceDetails(handle:ThingId, id:OID, display:String, ownerHandle:ThingId)
case class MySpaces(ownedByMe:Seq[SpaceDetails], memberOf:Seq[SpaceDetails]) extends ListMySpacesResponse

case class GetSpacesStatus(val requester:User) extends SpaceMgrMsg
case class SpaceStatus(spaceId:OID, name:String, thingConvs:Int, nSessions:Int)

// This responds eventually with a ThingFound:
case class CreateSpace(requester:User, name:String) extends SpaceMgrMsg

/**
 * The base class for message that get routed to a Space. Note that owner is only relevant if
 * the spaceId is AsName (which needs to be disambiguated by owner).
 */
sealed class SpaceMessage(val requester:User, val owner:OID, val spaceId:ThingId) extends SpaceMgrMsg
object SpaceMessage {
  def unapply(input:SpaceMessage) = Some((input.requester, input.owner, input.spaceId))
}

case class CreateThing(req:User, own:OID, space:ThingId, kind:Kind, modelId:OID, props:PropMap) extends SpaceMessage(req, own, space)

case class ModifyThing(req:User, own:OID, space:ThingId, id:ThingId, modelId:OID, props:PropMap) extends SpaceMessage(req, own, space)

/**
 * A specialized form of ModifyThing for the most common case, especially for internal use: changing a few specific properties.
 */
case class ChangeProps(req:User, own:OID, space:ThingId, id:ThingId, changedProps:PropMap) extends SpaceMessage(req, own, space)

// TODO: this message needs cleanup before we start using it, to match the rest:
case class CreateProperty(id:OID, req:User, model:OID, pType:OID, cType:OID, props:PropMap) extends SpaceMessage(req, UnknownOID, AsOID(id))

case class DeleteThing(req:User, own:OID, space:ThingId, thing:ThingId) extends SpaceMessage(req, own, space)

/**
 * All Conversation-oriented messages get wrapped in a ConversationRequest.
 */
case class ConversationRequest(req:User, own:OID, space:ThingId, payload:ConversationMessage) extends SpaceMessage(req, own, space)

/**
 * All User Session-oriented messages get wrapped in a SessionRequest.
 */
case class SessionRequest(req:User, own:OID, space:ThingId, payload:SessionMessage) extends SpaceMessage(req, own, space)

/**
 * TODO: HACK: this exposes the UserValues for a Space, outside that Space. It should really go away, but is necessary for
 * _userValues and _thingValues.
 */
case class UserValuePersistRequest(req:User, own:OID, space:ThingId, payload:querki.uservalues.PersistMessages.ExternallyExposed) extends SpaceMessage(req, own, space)

/**
 * An open-ended variant of SpaceMgrMsg, which gets routed to Space and can contain anything. This is intended
 * specifically for use by SpacePlugins.
 * 
 * (Why the indirection through payload? So that we can leave this mechanism open-ended, while still leaving SpaceMgrMsg and SpaceMessage
 * sealed here.)
 */
case class SpacePluginMsg(req:User, own:OID, space:ThingId, payload:Any) extends SpaceMessage(req, own, space) 

object SpaceError {  
  val CreateNotAllowed = "Space.createThing.notAllowed"
  val ModifyNotAllowed = "Space.modifyThing.notAllowed"
  val NameExists = "Space.createThing.nameExists"
  val UnknownID = "Thing.find.unknownId"
  val UnknownName = "Thing.find.unknownName"
  val UnknownPath = "Thing.find.noSuch"
}
import SpaceError._

// General message published from a Space to its subscribers. Possibly still a bit half-baked, but is likely to become
// important.
case class CurrentState(state:SpaceState)

case class InviteRequest(req:User, own:OID, space:ThingId, rc:RequestContext, emails:Seq[querki.email.EmailAddress], collabs:Seq[OID]) 
  extends SpaceMessage(req, own, space)
case class InviteResult(msg:String)

case class JoinRequest(req:User, own:OID, space:ThingId, rc:RequestContext) extends SpaceMessage(req, own, space)
sealed trait JoinResult
case object Joined extends JoinResult
case class JoinFailed(ex:PublicException) extends JoinResult

// This is the most common response when you create/fetch any sort of Thing
sealed trait SpaceResponse
sealed trait ThingResponse extends SpaceResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingError(ex:PublicException, stateOpt:Option[SpaceState] = None) extends ThingResponse
