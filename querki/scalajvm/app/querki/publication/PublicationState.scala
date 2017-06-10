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
case class PublicationState(events:Vector[RawPublishEvent], publishingRSS:Boolean)(implicit val ecology:Ecology) extends EcologyMember {
  
  lazy val Publication = interface[Publication]
  
  lazy val currentRSS:String = ???
  
  import PublicationCommands.{OnePublishEvent, OnePublishedThing}
  
  /**
   * This is a memo'ized transformation of the raw event stream to the "public" form returned by
   * GetEvents. The primary difference is that it puzzles out the difference between Publish and
   * Update.
   */
  lazy val publicEvents:Vector[OnePublishEvent] = {
    // We iterate through, building up the translated events *and* a map of previous publications, so that each
    // event knows about the previous state of this Thing:
    val result = ((Vector.empty[OnePublishEvent], Map.empty[OID, PublishedThingInfo]) /: events) { case ((evts, prevStates), evt) =>
      val RawPublishEvent(who, things, meta, when) = evt
      val (pubThings, newStates) = ((Seq.empty[OnePublishedThing], prevStates) /: things) { case ((ts, states), (oid, thingInfo)) =>
        val prevState = prevStates.get(oid)
        // TODO: when we get to diffs, this is where we calculate it, and stick it into this structure:
        val pubThing = OnePublishedThing(
          oid,
          prevState.isDefined,
          thingInfo.title.getOrElse(oid.toString),
          thingInfo.display
        )
        (ts :+ pubThing, states + (oid -> thingInfo))
      }
      val pubEvt = OnePublishEvent(
        who,
        when,
        // TBD: this is a horrible hack, but doing it properly requires the Ecology. Is it worth it?
        meta.contains(MOIDs.MinorUpdateOID),
        pubThings,
        meta.getFirstOpt(Publication.PublishNotesProp)
      )
      (evts :+ pubEvt, newStates)
    }
    
    result._1
  }
}
