package querki.spaces.messages

import language.implicitConversions
import models._
import Kind._
import MIMEType.MIMEType
import models.{ThingId, UnknownOID, OID, AsOID}
import querki.conversations.messages.ConversationMessage
import querki.history.HistoryFunctions.SetStateReason
import querki.identity.{IdentityId, User, PublicIdentity}
import querki.session.messages.SessionMessage
import querki.spaces.SpaceMessagePersistence.SpaceEvent
import querki.spaces.{StatusNormal, SpaceStatusCode}
import querki.values.{SpaceVersion, SpaceState, RequestContext}
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
case class CreateSpace(rc: RequestContext, name:String, initialStatus:SpaceStatusCode = StatusNormal) extends SpaceMgrMsg

case class ChangeSpaceStatus(spaceId:OID, newStatus:SpaceStatusCode) extends SpaceMgrMsg
case object StatusChanged

/**
 * The base class for message that get routed to a Space.
 */
sealed class SpaceMessage(val requester:User, val spaceId:OID) extends Serializable
object SpaceMessage {
  def unapply(input:SpaceMessage) = Some((input.requester, input.spaceId))
}

case class GetSpaceInfo(req:User, space:OID) extends SpaceMessage(req, space)
  
case class SpaceId(id:OID)
case class SpaceInfo(id:OID, linkName:String, display:String, ownerHandle:String)

/**
 * Create something.
 * 
 * IMPORTANT: the "localCall" parameter indicates that this request is coming from an Actor in the Space's
 * own troupe. Iff not true, it means that we can't respond with a ThingFound(), so we respond with a simpler
 * ThingAck() instead.
 * 
 * If thingIdOpt is set, then we are re-creating a previously existing Thing from the History.
 */
case class CreateThing(rc: RequestContext, space:OID, kind:Kind, modelId:OID, props:PropMap, thingIdOpt:Option[OID] = None, localCall:Boolean = true) extends SpaceMessage(rc.requesterOrAnon, space)

/**
 * TODO: this is largely redundant with ChangeProps and ChangeModel at this point. It should be removed.
 */
case class ModifyThing(rc: RequestContext, space:OID, id:ThingId, modelId:OID, props:PropMap, localCall:Boolean = true) extends SpaceMessage(rc.requesterOrAnon, space)

case class ChangeModel(rc: RequestContext, space:OID, id:ThingId, newModelId:OID, localCall:Boolean = true) extends SpaceMessage(rc.requesterOrAnon, space)

/**
 * A specialized form of ModifyThing for the most common case, especially for internal use: changing a few specific properties.
 */
case class ChangeProps(rc: RequestContext, space:OID, id:ThingId, changedProps:PropMap, localCall:Boolean = true) extends SpaceMessage(rc.requesterOrAnon, space)

case class DeleteThing(rc: RequestContext, space:OID, thing:ThingId) extends SpaceMessage(rc.requesterOrAnon, space)

/**
 * This is the initialization message for a newly-created Space, with the basic starting info.
 * It may *only* be sent to a newly-created Space: it is an error if the Space already has state!
 * 
 * Note that we only send the owning Identity's OID; it can then be fetched from req.
 */
case class InitialState(rc: RequestContext, space:OID, display:String, owner:IdentityId) extends SpaceMessage(rc.requesterOrAnon, space)
/**
 * This is the ack from InitialState. We specifically do *not* respond with ThingFound in this case, because
 * the message often goes cross-node.
 */
case object StateInitialized

/**
 * Sets the state of this Space to a new value. This is sometimes sent at creation time (for imported Spaces),
 * sometimes after major changes. Either way, it may *ONLY* be sent from within the Space's own troupe, because
 * it contains a SpaceState!
 */
case class SetState(rc: RequestContext, space:OID, state:SpaceState, reason:SetStateReason, details:String) extends SpaceMessage(rc.requesterOrAnon, space)

/**
 * The "payload" in a SpaceSubsystemRequest. Note that this is intentionally and necessarily *not* sealed --
 * these types are scattered around in various subsystems.
 * 
 * TBD: in a perfect world (that is, Dotty), we might instead define this as a type union of those scattered
 * types. But I'm not sure what's better.
 */
trait SpaceMessagePayload
/**
 * A message that is intended to be routed to a different member of the Space's troupe.
 */
case class SpaceSubsystemRequest(rc: RequestContext, space:OID, payload:SpaceMessagePayload) extends SpaceMessage(rc.requesterOrAnon, space)

/**
 * An open-ended variant of SpaceMgrMsg, which gets routed to Space and can contain anything. This is intended
 * specifically for use by SpacePlugins, and gets routed through the Space Actor itself, which is why it is
 * separate from SpaceSubsystemRequest.
 * 
 * TBD: can/should these messages be unified? Maybe...
 */
case class SpacePluginMsg(rc: RequestContext, space:OID, payload:Any) extends SpaceMessage(rc.requesterOrAnon, space)
  
/**
 * Launch a PhotoUploadActor.
 */
case class BeginProcessingPhoto(rc: RequestContext, space:OID, mimeType:Option[String]) extends SpaceMessage(rc.requesterOrAnon, space)
case object ImageComplete

/**
 * Tells this Space to pro-actively close itself.
 */
case class ShutdownSpace(rc: RequestContext, space:OID) extends SpaceMessage(rc.requesterOrAnon, space)
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

/**
  * The current or updated state of this Space.
  *
  * If `events` is present, it is the list of changes that led to this new State. Clients may use this list to
  * evolve their own view of the state, instead of using the new raw State.
  *
  * @param state The updated state of this Space
  * @param events The events that caused this update.
  */
case class CurrentState(state: SpaceState, events: Option[List[SpaceEvent]] = None)

sealed trait SpaceMembersBase extends SpaceMessagePayload

case class InviteRequest(rc:RequestContext, emails:Seq[querki.email.EmailAddress], collabs:Seq[OID]) 
  extends SpaceMembersBase
case class InviteResult(msg:String)

case class JoinRequest(rc:RequestContext, personId:OID) extends SpaceMembersBase
sealed trait JoinResult
case object Joined extends JoinResult
case class JoinFailed(ex:PublicException) extends JoinResult

case class JoinByOpenInvite(rc:RequestContext, roleId:OID) extends SpaceMembersBase

case class IsSpaceMemberP(rc:RequestContext) extends SpaceMembersBase
case class IsSpaceMember(result:Boolean)

case class ReplacePerson(guestId:OID, actualIdentity:PublicIdentity) extends SpaceMembersBase
case object PersonReplaced

// This is the most common response when you create/fetch any sort of Thing
sealed trait SpaceResponse
sealed trait ThingResponse extends SpaceResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingError(ex:PublicException, stateOpt:Option[SpaceState] = None) extends ThingResponse
// Specialized variant of ThingFound, solely for cross-node acknowledgement
// TODO: this sucks. Come up with a better protocol, possibly using Akka Typed for inspiration.
case class ThingAck(id:OID)