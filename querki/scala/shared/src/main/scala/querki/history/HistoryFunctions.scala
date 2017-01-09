package querki.history

import scala.concurrent.Future

import enumeratum.EnumEntry._
import enumeratum.values._

import models._
import querki.data._
import querki.time.Common.Timestamp

trait HistoryFunctions {
  import HistoryFunctions._
  
  /**
   * Fetch the complete history of this Space.
   */
  def getHistorySummary():Future[HistorySummary]
  
  /**
   * Rolls this Space back to the specified version. The UI should do confirmation first!
   */
  def rollbackTo(v:HistoryVersion):Future[SpaceInfo]
  
  /**
   * Restores the specified Thing, as of the last version where it existed.
   * 
   * We assume that the given TID is specifically from the oid field.
   */
  def restoreDeletedThing(tid:TID):Future[ThingInfo]
}

object HistoryFunctions {
  
  type HistoryVersion = Long
  
  /**
   * Enumeration of the reasons why this Space's State was slammed.
   * 
   * @param value The concrete enum value that gets persisted.
   */
  sealed abstract class SetStateReason(val value:Int, val msgName:String) extends IntEnumEntry
  case object SetStateReason extends IntEnum[SetStateReason] with IntUPickleEnum[SetStateReason] {
    val values = findValues
    
    case object Unknown extends SetStateReason(value = 0, msgName = "unknown")
    case object ImportedFromMySQL extends SetStateReason(value = 1, msgName = "importFromOld")
    case object ExtractedAppFromHere extends SetStateReason(value = 2, msgName = "extractedApp")
    case object InitialAppState extends SetStateReason(value = 3, msgName = "initialAppState")
    case object RolledBack extends SetStateReason(value = 4, msgName = "rolledBack")
  }
  
  /**
   * The subtypes of EvtSummary represent summary descriptions of history events.
   */
  sealed trait EvtSummary {
    /**
     * The index of this particular event.
     */
    def idx:Long
    /**
     * Who did this. This is an index into the whoMap.
     * 
     * TODO: bad smell! This should be something more strongly-typed!
     */
    def who:String
    /**
     * When this happened.
     */
    def time:Timestamp
  }
  case class SetStateSummary(idx:HistoryVersion, who:String, time:Timestamp, reason:SetStateReason, details:String) extends EvtSummary
  case class ImportSummary(idx:HistoryVersion, who:String, time:Timestamp) extends EvtSummary
  case class CreateSummary(idx:HistoryVersion, who:String, time:Timestamp, kind:Kind.Kind, id:TID, model:TID, restored:Boolean) extends EvtSummary
  case class ModifySummary(idx:HistoryVersion, who:String, time:Timestamp, id:TID, props:Seq[TID]) extends EvtSummary
  case class DeleteSummary(idx:HistoryVersion, who:String, time:Timestamp, id:TID) extends EvtSummary
  case class AddAppSummary(idx:HistoryVersion, who:String, time:Timestamp, appId:TID) extends EvtSummary
  
  type IdentityMap = Map[String, IdentityInfo]
  type ThingNames = Map[TID, String]
  
  case class EvtContext(whoMap:IdentityMap, thingNames:ThingNames)
  
  /**
   * The full history of this Space, in summary form.
   */
  case class HistorySummary(events:Seq[EvtSummary], context:EvtContext)
}
