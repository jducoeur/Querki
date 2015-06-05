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

case class GetSpaceCount(requester:User) extends SpaceMgrMsg
case class SpaceCount(count:Long)

case class GetSpacesStatus(val requester:User) extends SpaceMgrMsg
case class SpaceStatus(spaceId:OID, name:String, thingConvs:Int, nSessions:Int)

// This responds eventually with a ThingFound:
case class CreateSpace(requester:User, name:String) extends SpaceMgrMsg

/**
 * The base class for message that get routed to a Space.
 * 
 * TODO: owner is now vestigial -- remove it.
 */
sealed class SpaceMessage(val requester:User, val owner:OID, val spaceId:OID)
object SpaceMessage {
  def unapply(input:SpaceMessage) = Some((input.requester, input.owner, input.spaceId))
}
  
case class SpaceId(id:OID)
case class SpaceInfo(id:OID, linkName:String)

case class CreateThing(req:User, own:OID, space:OID, kind:Kind, modelId:OID, props:PropMap) extends SpaceMessage(req, own, space)

case class ModifyThing(req:User, own:OID, space:OID, id:ThingId, modelId:OID, props:PropMap) extends SpaceMessage(req, own, space)

/**
 * A specialized form of ModifyThing for the most common case, especially for internal use: changing a few specific properties.
 */
case class ChangeProps(req:User, own:OID, space:OID, id:ThingId, changedProps:PropMap) extends SpaceMessage(req, own, space)

// TODO: this message needs cleanup before we start using it, to match the rest:
case class CreateProperty(id:OID, req:User, model:OID, pType:OID, cType:OID, props:PropMap) extends SpaceMessage(req, UnknownOID, id)

case class DeleteThing(req:User, own:OID, space:OID, thing:ThingId) extends SpaceMessage(req, own, space)

/**
 * All Conversation-oriented messages get wrapped in a ConversationRequest.
 */
case class ConversationRequest(req:User, own:OID, space:OID, payload:ConversationMessage) extends SpaceMessage(req, own, space)

/**
 * All User Session-oriented messages get wrapped in a SessionRequest.
 */
case class SessionRequest(req:User, own:OID, space:OID, payload:SessionMessage) extends SpaceMessage(req, own, space)

/**
 * TODO: HACK: this exposes the UserValues for a Space, outside that Space. It should really go away, but is necessary for
 * _userValues and _thingValues.
 */
case class UserValuePersistRequest(req:User, own:OID, space:OID, payload:querki.uservalues.PersistMessages.ExternallyExposed) extends SpaceMessage(req, own, space)

/**
 * An open-ended variant of SpaceMgrMsg, which gets routed to Space and can contain anything. This is intended
 * specifically for use by SpacePlugins.
 * 
 * (Why the indirection through payload? So that we can leave this mechanism open-ended, while still leaving SpaceMgrMsg and SpaceMessage
 * sealed here.)
 */
case class SpacePluginMsg(req:User, own:OID, space:OID, payload:Any) extends SpaceMessage(req, own, space) 

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

case class SpaceMembersMessage(req:User, own:OID, space:OID, msg:SpaceMembersBase) extends SpaceMessage(req, own, space)
sealed trait SpaceMembersBase

case class InviteRequest(rc:RequestContext, emails:Seq[querki.email.EmailAddress], collabs:Seq[OID]) 
  extends SpaceMembersBase
case class InviteResult(msg:String)

case class JoinRequest(rc:RequestContext) extends SpaceMembersBase
sealed trait JoinResult
case object Joined extends JoinResult
case class JoinFailed(ex:PublicException) extends JoinResult

// This is the most common response when you create/fetch any sort of Thing
sealed trait SpaceResponse
sealed trait ThingResponse extends SpaceResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingError(ex:PublicException, stateOpt:Option[SpaceState] = None) extends ThingResponse
