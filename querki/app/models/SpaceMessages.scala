package models

import language.implicitConversions
import Kind._
import Thing._

import querki.identity.User

import system._

import MIMEType.MIMEType

/**
 * ThingId deals with the fact that we sometimes are passing OIDs around in messages, and
 * sometimes OIDs. This provides an abstraction that lets you use either in a sensible way.
 */
sealed trait ThingId
case class AsOID(oid:OID) extends ThingId {
  override def toString() = "." + oid.toString
}
case class AsName(name:String) extends ThingId {
  // We are currently using toUrl for this, *not* canonicalize, because we want to preserve case
  // in the URL. This does, however, mean that you can't casually compare the toString'ed versions
  // of two AsNames and assume that they are equal!
  override def toString() = NameType.toUrl(name)
}
object UnknownThingId extends AsOID(UnknownOID)
object ThingId {
  def apply(str:String):ThingId = {
    str(0) match {
      case '.' => AsOID(OID(str.substring(1)))
      case _ => AsName(str)
    }
  }
  
  implicit def thingId2Str(id:ThingId) = id.toString()
}

sealed trait SpaceMgrMsg

case class ListMySpaces(owner:OID) extends SpaceMgrMsg
sealed trait ListMySpacesResponse
case class MySpaces(spaces:Seq[(AsName,AsOID,String)]) extends ListMySpacesResponse

// This responds eventually with a ThingFound:
case class CreateSpace(owner:OID, name:String) extends SpaceMgrMsg

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

case class GetThing(req:User, own:OID, space:ThingId, thing:Option[ThingId]) extends SpaceMessage(req, own, space)

object SpaceError extends Enumeration {
  type SpaceError = Value
  
  val CreateNotAllowed, IllegalName, ModifyNotAllowed, NameExists, SpaceNotFound, UnknownID, UnknownName, UnknownPath = Value
}
import SpaceError._

// This is the most common response when you create/fetch any sort of Thing
sealed trait ThingResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingFailed(error:SpaceError, msg:String, stateOpt:Option[SpaceState] = None) extends ThingResponse

sealed trait AttachmentResponse
case class AttachmentContents(id:OID, size:Int, mime:MIMEType, content:Array[Byte]) extends AttachmentResponse
case class AttachmentFailed() extends AttachmentResponse

