package querki.history

import scala.concurrent.Future

import upickle.default.{macroRW, ReadWriter => RW}

import models._
import querki.data._
import querki.time.Common.Timestamp

import upickle.default._

trait HistoryFunctions {
  import HistoryFunctions._

  /**
   * Fetch some of the history of this Space.
   *
   * @param end the record to finish at, or None to end at the most recent version
   * @param nRecords how many records to fetch (capped at the server end, to prevent abuse)
   */
  def getHistorySummary(
    end: Option[HistoryVersion],
    nRecords: Int
  ): Future[HistorySummary]

  /**
   * Rolls this Space back to the specified version. The UI should do confirmation first!
   */
  def rollbackTo(v: HistoryVersion): Future[SpaceInfo]

  /**
   * Restores the specified Thing, as of the last version where it existed.
   *
   * We assume that the given TID is specifically from the oid field.
   */
  def restoreDeletedThing(tid: TID): Future[ThingInfo]
}

object HistoryFunctions {

  type HistoryVersion = Long

  val viewingHistoryParam = "_historyVersion"

  /**
   * Enumeration of the reasons why this Space's State was slammed.
   *
   * @param value The concrete enum value that gets persisted.
   */
  sealed abstract class SetStateReason(
    val value: Int,
    val msgName: String
  )

  case object SetStateReason {
    case object Unknown extends SetStateReason(value = 0, msgName = "unknown")
    case object ImportedFromMySQL extends SetStateReason(value = 1, msgName = "importFromOld")
    case object ExtractedAppFromHere extends SetStateReason(value = 2, msgName = "extractedApp")
    case object InitialAppState extends SetStateReason(value = 3, msgName = "initialAppState")
    case object RolledBack extends SetStateReason(value = 4, msgName = "rolledBack")
    case object ImportedFromExport extends SetStateReason(value = 5, msgName = "importFromExport")

    val items = List(Unknown, ImportedFromMySQL, ExtractedAppFromHere, InitialAppState, RolledBack, ImportedFromExport)

    lazy val itemsByValue: Map[Int, SetStateReason] =
      items.map(i => (i.value -> i)).toMap

    implicit val rw: ReadWriter[SetStateReason] = readwriter[Int].bimap(_.value, itemsByValue(_))
  }

  /**
   * The subtypes of EvtSummary represent summary descriptions of history events.
   */
  sealed trait EvtSummary {

    /**
     * The index of this particular event.
     */
    def idx: Long

    /**
     * Who did this. This is an index into the whoMap.
     *
     * TODO: bad smell! This should be something more strongly-typed!
     */
    def who: String

    /**
     * When this happened.
     */
    def time: Timestamp
  }

  case class SetStateSummary(
    idx: HistoryVersion,
    who: String,
    time: Timestamp,
    reason: SetStateReason,
    details: String
  ) extends EvtSummary

  case class ImportSummary(
    idx: HistoryVersion,
    who: String,
    time: Timestamp
  ) extends EvtSummary

  case class CreateSummary(
    idx: HistoryVersion,
    who: String,
    time: Timestamp,
    kind: Kind.Kind,
    id: TID,
    model: TID,
    restored: Boolean
  ) extends EvtSummary

  case class ModifySummary(
    idx: HistoryVersion,
    who: String,
    time: Timestamp,
    id: TID,
    props: Seq[TID]
  ) extends EvtSummary

  case class DeleteSummary(
    idx: HistoryVersion,
    who: String,
    time: Timestamp,
    id: TID
  ) extends EvtSummary

  case class AddAppSummary(
    idx: HistoryVersion,
    who: String,
    time: Timestamp,
    appId: TID
  ) extends EvtSummary

  object EvtSummary {
    implicit val sssrw: RW[SetStateSummary] = macroRW
    implicit val isrw: RW[ImportSummary] = macroRW
    implicit val csrw: RW[CreateSummary] = macroRW
    implicit val msrw: RW[ModifySummary] = macroRW
    implicit val dsrw: RW[DeleteSummary] = macroRW
    implicit val aasrw: RW[AddAppSummary] = macroRW
    implicit val rw: RW[EvtSummary] = macroRW
  }

  type IdentityMap = Map[String, IdentityInfo]
  type ThingNames = Map[TID, String]

  case class EvtContext(
    whoMap: IdentityMap,
    thingNames: ThingNames
  )

  object EvtContext {
    implicit val rw: RW[EvtContext] = macroRW
  }

  /**
   * The full history of this Space, in summary form.
   */
  case class HistorySummary(
    events: Seq[EvtSummary],
    context: EvtContext
  )

  object HistorySummary {
    implicit val rw: RW[HistorySummary] = macroRW
  }
}
