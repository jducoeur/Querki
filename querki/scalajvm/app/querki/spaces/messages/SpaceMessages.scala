package querki.spaces.messages

import language.implicitConversions

import models.{Kind, MIMEType, Thing}
import Kind._
import MIMEType.MIMEType
import models.Thing.PropMap
import models.{AsOID, OID, ThingId, UnknownOID}

import querki.conversations.messages.ConversationMessage
import querki.identity.{IdentityId, User}
import querki.session.messages.SessionMessage
import querki.spaces.{SpaceStatusCode, StatusNormal}
import querki.values.{RequestContext, SpaceState, SpaceVersion}
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

/**
 * Builds a new Space with the given name. Responds with a ThingFound once the Space is ready to
 * interact with.
 * 
 * @param initialStatus The status that the Space will be started with. If this is anything *other*
 * than StatusNormal, the Space will be shut down immediately after creation, so that the caller
 * can deal with post-creation operations.
 */
case class CreateSpace(requester:User, name:String, initialStatus:SpaceStatusCode = StatusNormal) extends SpaceMgrMsg

case class ChangeSpaceStatus(spaceId:OID, newStatus:SpaceStatusCode) extends SpaceMgrMsg
case object StatusChanged

/**
 * The base class for message that get routed to a Space.
 * 
 * TODO: owner is now vestigial -- remove it.
 */
sealed class SpaceMessage(val requester:User, val spaceId:OID) extends Serializable
object SpaceMessage {
  def unapply(input:SpaceMessage) = Some((input.requester, input.spaceId))
}

case class GetSpaceInfo(req:User, space:OID) extends SpaceMessage(req, space)
  
case class SpaceId(id:OID)
case class SpaceInfo(id:OID, linkName:String, display:String, ownerHandle:String)

case class CreateThing(req:User, space:OID, kind:Kind, modelId:OID, props:PropMap) extends SpaceMessage(req, space)

case class ModifyThing(req:User, space:OID, id:ThingId, modelId:OID, props:PropMap) extends SpaceMessage(req, space)

/**
 * A specialized form of ModifyThing for the most common case, especially for internal use: changing a few specific properties.
 */
case class ChangeProps(req:User, space:OID, id:ThingId, changedProps:PropMap, sync:Boolean = false) extends SpaceMessage(req, space)

case class DeleteThing(req:User, space:OID, thing:ThingId) extends SpaceMessage(req, space)

/**
 * This is the initialization message for a newly-created Space, with the basic starting info.
 * It may *only* be sent to a newly-created Space: it is an error if the Space already has state!
 * 
 * Note that we only send the owning Identity's OID; it can then be fetched from req.
 */
case class InitialState(req:User, space:OID, display:String, owner:IdentityId) extends SpaceMessage(req, space)

/**
 * All Conversation-oriented messages get wrapped in a ConversationRequest.
 */
case class ConversationRequest(req:User, space:OID, payload:ConversationMessage) extends SpaceMessage(req, space)

/**
 * All User Session-oriented messages get wrapped in a SessionRequest.
 */
case class SessionRequest(req:User, space:OID, payload:SessionMessage) extends SpaceMessage(req, space)

/**
 * TODO: HACK: this exposes the UserValues for a Space, outside that Space. It should really go away, but is necessary for
 * _userValues and _thingValues.
 */
case class UserValuePersistRequest(req:User, space:OID, payload:querki.uservalues.PersistMessages.ExternallyExposed) extends SpaceMessage(req, space)

/**
 * An open-ended variant of SpaceMgrMsg, which gets routed to Space and can contain anything. This is intended
 * specifically for use by SpacePlugins.
 * 
 * (Why the indirection through payload? So that we can leave this mechanism open-ended, while still leaving SpaceMgrMsg and SpaceMessage
 * sealed here.)
 */
case class SpacePluginMsg(req:User, space:OID, payload:Any) extends SpaceMessage(req, space) 
  
/**
 * Launch a PhotoUploadActor.
 */
case class BeginProcessingPhoto(req:User, space:OID, mimeType:Option[String]) extends SpaceMessage(req, space) 
case object ImageComplete

/**
 * Tells this Space to pro-actively close itself.
 */
case class ShutdownSpace(req:User, space:OID) extends SpaceMessage(req, space)
case object ShutdownAck

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

case class SpaceMembersMessage(req:User, space:OID, msg:SpaceMembersBase) extends SpaceMessage(req, space)
sealed trait SpaceMembersBase

case class InviteRequest(rc:RequestContext, emails:Seq[querki.email.EmailAddress], collabs:Seq[OID]) 
  extends SpaceMembersBase
case class InviteResult(msg:String)

case class JoinRequest(rc:RequestContext, personId:OID) extends SpaceMembersBase
sealed trait JoinResult
case object Joined extends JoinResult
case class JoinFailed(ex:PublicException) extends JoinResult

case class IsSpaceMemberP(rc:RequestContext) extends SpaceMembersBase
case class IsSpaceMember(result:Boolean)

// This is the most common response when you create/fetch any sort of Thing
sealed trait SpaceResponse
sealed trait ThingResponse extends SpaceResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingError(ex:PublicException, stateOpt:Option[SpaceState] = None) extends ThingResponse
