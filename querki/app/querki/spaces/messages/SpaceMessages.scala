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
import querki.values.SpaceState
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

case class CreateAttachment(req:User, own:OID, space:ThingId, 
    content:Array[Byte], mime:MIMEType, size:Int, 
    modelId:OID, props:PropMap) extends SpaceMessage(req, own, space)

case class GetAttachment(req:User, own:OID, space:ThingId, attachId:ThingId) extends SpaceMessage(req, own, space)

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

// This is the most common response when you create/fetch any sort of Thing
sealed trait SpaceResponse
sealed trait ThingResponse extends SpaceResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingError(ex:PublicException, stateOpt:Option[SpaceState] = None) extends ThingResponse
case class AttachmentContents(id:OID, size:Int, mime:MIMEType, content:Array[Byte]) extends SpaceResponse
