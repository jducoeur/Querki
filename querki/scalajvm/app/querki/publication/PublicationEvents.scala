package querki.publication

import models.ModelPersistence._
import querki.globals._
import querki.identity.IdentityId
import querki.persistence._
import querki.time.DateTime

/**
 * The Events that actually get evolved and persisted in the Publication system.
 */
object PublicationEvents {
  
  case class PublishedThingInfo(
    @KryoTag(1) thingId:OID, 
    @KryoTag(2) display:String, 
    @KryoTag(3) strip:String,
    @KryoTag(4) title:AddedField[String]) extends UseKryo
  
  sealed trait PublicationEvent
  
  /**
   * Note that this covers both Publish and Update commands -- in most respects, they're
   * identical.
   */
  case class PublishEvent(
    @KryoTag(1) who:IdentityId, 
    @KryoTag(2) things:Seq[PublishedThingInfo], 
    @KryoTag(3) meta:DHPropMap,
    @KryoTag(4) when:DateTime) extends UseKryo with PublicationEvent
}
