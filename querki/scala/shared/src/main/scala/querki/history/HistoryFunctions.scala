package querki.history

import scala.concurrent.Future

import models._
import querki.data._
import querki.time.Common.Timestamp

trait HistoryFunctions {
  import HistoryFunctions._
  
  /**
   * Fetch the complete history of this Space.
   */
  def getHistorySummary():Future[HistorySummary]
}

object HistoryFunctions {
  
  type HistoryVersion = Long
  
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
  case class BootSummary(idx:HistoryVersion, who:String, time:Timestamp) extends EvtSummary
  case class ImportSummary(idx:HistoryVersion, who:String, time:Timestamp) extends EvtSummary
  case class CreateSummary(idx:HistoryVersion, who:String, time:Timestamp, kind:Kind.Kind, id:TID, model:TID) extends EvtSummary
  case class ModifySummary(idx:HistoryVersion, who:String, time:Timestamp, id:TID, props:Seq[TID]) extends EvtSummary
  case class DeleteSummary(idx:HistoryVersion, who:String, time:Timestamp, id:TID) extends EvtSummary
  
  type IdentityMap = Map[String, IdentityInfo]
  type ThingNames = Map[TID, String]
  
  case class EvtContext(whoMap:IdentityMap, thingNames:ThingNames)
  
  /**
   * The full history of this Space, in summary form.
   */
  case class HistorySummary(events:Seq[EvtSummary], context:EvtContext)
}
