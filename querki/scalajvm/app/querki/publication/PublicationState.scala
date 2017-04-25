package querki.publication

import models._
import querki.globals._
import querki.identity.{PublicIdentity, User}
import querki.time.DateTime

import PublicationEvents._

/**
 * The fully-spelled-out version of a Publish or Update, as we keep it in memory.
 */
case class RawPublishEvent(identity:PublicIdentity, things:Map[OID, PublishedThingInfo], meta:PropMap, when:DateTime)

/**
 * This represents the Publication history of a given Space.
 */
case class PublicationState(events:Vector[RawPublishEvent]) {
  lazy val currentRSS:String = ???
}
