package querki.publication

import models.ModelPersistence._
import querki.globals._
import querki.identity.IdentityPersistence.UserRef
import querki.persistence._

/**
 * The Events that actually get evolved and persisted in the Publication system.
 */
object PublicationEvents {
  sealed trait PublicationEvent
  
  /**
   * Note that this covers both Publish and Update commands -- in most respects, they're
   * identical.
   */
  case class PublishEvent(
    @KryoTag(1) who:UserRef, 
    @KryoTag(2) things:Seq[DHThingState], 
    @KryoTag(3) meta:DHPropMap) extends UseKryo with PublicationEvent
}
