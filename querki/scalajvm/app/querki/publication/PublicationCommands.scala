package querki.publication

import models._
import querki.core.QLText
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
    things:Seq[OnePublishedThing],
    notes:Option[QLText]
  )
  
  /**
   * The events that correspond to a GetEvents request: this is a series of events, in chrono order. Note that this may be empty!
   */
  case class RequestedEvents(events:Seq[OnePublishEvent])
  
  /**
   * Sent when the user wants to subscribe to this Space's RSS feed.
   * 
   * This has the side-effect of turning RSS on, and can take a bit of extra time when it is first hit.
   */
  case class GetRSSUrl(who:User) extends PublicationCommand

  /**
   * The response to GetRSSUrl.
   */
  case class RSSUrl(url:String)
}
