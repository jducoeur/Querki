package querki.publication

import models._
import querki.globals._
import querki.identity.User
import querki.time.DateTime

/**
 * The Commands that can legitimately be sent to the PublicationCore.
 */
object PublicationCommands {
  sealed trait PublicationCommand
  
  case class Publish(who:User, things:Seq[OID], meta:PropMap, state:SpaceState) extends PublicationCommand
  case class Update(who:User, things:Seq[OID], meta:PropMap, state:SpaceState) extends PublicationCommand
  case class GetEvents(
    who:User, 
    since:DateTime, 
    until:DateTime, 
    changesTo:Seq[OID], 
    includeMinor:Boolean, 
    viewAsPublic:Boolean, 
    coalesce:Boolean) extends PublicationCommand
}
