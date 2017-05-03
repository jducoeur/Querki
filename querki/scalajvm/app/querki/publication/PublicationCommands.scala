package querki.publication

import models._
import querki.globals._
import querki.identity.{PublicIdentity, User}
import querki.time.DateTime

/**
 * The Commands that can legitimately be sent to the PublicationCore.
 */
object PublicationCommands {
  sealed trait PublicationCommand extends querki.spaces.messages.SpaceMessagePayload
  
  case class Publish(who:User, things:Seq[OID], meta:PropMap, state:SpaceState) extends PublicationCommand
  case class Update(who:User, things:Seq[OID], meta:PropMap, state:SpaceState) extends PublicationCommand
  
  /**
   * Returned in response to Publish and Update -- this is the State that results after Publication.
   */
  case class PublishResponse(updatedState:SpaceState)
  
  case class GetEvents(
    who:User, 
    since:Option[DateTime], 
    until:Option[DateTime], 
    changesTo:Set[OID], 
    includeMinor:Boolean, 
    coalesce:Boolean) extends PublicationCommand
    
  case class OnePublishedThing(
    thingId:OID,
    isUpdate:Boolean,
    displayName:String,
    display:String
  )
  
  case class OnePublishEvent(
    who:PublicIdentity,
    when:DateTime,
    isMinor:Boolean,
    things:Seq[OnePublishedThing]
  )
  
  /**
   * The events that correspond to a GetEvents request: this is a series of events, in chrono order. Note that this may be empty!
   */
  case class RequestedEvents(events:Seq[OnePublishEvent])
}
