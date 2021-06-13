package querki.history

import org.querki.requester.{RequestM, Requester}
import querki.data.TID

import scala.collection.immutable.Queue
import querki.values.{RequestContext, SpaceState}
import querki.globals._
import querki.history.HistoryFunctions.{
  AddAppSummary,
  CreateSummary,
  DeleteSummary,
  EvtContext,
  EvtSummary,
  HistorySummary,
  HistoryVersion,
  ImportSummary,
  ModifySummary,
  SetStateReason,
  SetStateSummary,
  ThingNames
}
import querki.identity.IdentityPersistence.UserRef
import querki.spaces.SpaceMessagePersistence._
import querki.time._

/**
 * This encapsulates the ability to build a summary of a Space's History, for sending to the front end.
 */
trait HistorySummaryBuilder extends EcologyMember with Requester with HistoryFolding {

  private lazy val ClientApi = interface[querki.api.ClientApi]
  private lazy val IdentityAccess = interface[querki.identity.IdentityAccess]

  case class HistoryScanState(
    evts: Queue[EvtSummary],
    identityIds: Set[OID],
    thingsIds: Set[OID],
    state: SpaceState
  )

  implicit def OID2TID(oid: OID): TID = ClientApi.OID2TID(oid)

  def toSummary(
    evt: SpaceEvent,
    sequenceNr: Long,
    identities: Set[OID],
    allThingIds: Set[OID]
  )(implicit
    state: SpaceState
  ): (EvtSummary, Set[OID], Set[OID]) = {
    def fill(
      userRef: UserRef,
      thingIds: Seq[OID],
      f: String => EvtSummary
    ): (EvtSummary, Set[OID], Set[OID]) = {
      val summary = f(userRef.identityIdOpt.map(_.toThingId.toString).getOrElse(""))

      val newIdentities = userRef.identityIdOpt match {
        case Some(identityId) => identities + identityId
        case None             => identities
      }

      val namePairs = for {
        thingId <- thingIds
        thing <- state.anything(thingId)
      } yield (thingId, thing.displayName)

      val newThingIds =
        (allThingIds /: namePairs) { (tn, pair) =>
          val (id, name) = pair
          tn + id
        }

      (summary, newIdentities, newThingIds)
    }

    evt match {
      case DHSetState(dh, modTime, reason, details) =>
        (
          SetStateSummary(
            sequenceNr,
            "",
            modTime.toTimestamp,
            SetStateReason.withValue(reason.getOrElse(0)),
            details.getOrElse("")
          ),
          identities,
          allThingIds
        )

      case DHInitState(userRef, display) => fill(userRef, Seq.empty, ImportSummary(sequenceNr, _, 0))

      case DHCreateThing(req, thingId, kind, modelId, dhProps, modTime, restored) =>
        fill(
          req,
          dhProps.keys.toSeq :+ thingId,
          CreateSummary(sequenceNr, _, modTime.toTimestamp, kind, thingId, modelId, restored.getOrElse(false))
        )

      case DHModifyThing(req, thingId, modelIdOpt, propChanges, replaceAllProps, modTime) =>
        fill(
          req,
          propChanges.keys.toSeq :+ thingId,
          ModifySummary(sequenceNr, _, modTime.toTimestamp, thingId, propChanges.keys.toSeq.map(OID2TID))
        )

      case DHDeleteThing(req, thingId, modTime) =>
        fill(req, Seq(thingId), DeleteSummary(sequenceNr, _, modTime.toTimestamp, thingId))

      case DHAddApp(req, modTime, app, appParents, shadowMapping, _) =>
        fill(req, Seq(app.id), AddAppSummary(sequenceNr, _, modTime.toTimestamp, app.id))
    }
  }

  def scanHistory(
    endOpt: Option[HistoryVersion],
    maxRecords: Int
  ): Future[HistoryScanState] = {
    val end = endOpt.getOrElse(Long.MaxValue)
    foldOverPartialHistory(1, end)(HistoryScanState(Queue.empty, Set.empty, Set.empty, emptySpace)) {
      (scanState, historyEvt) =>
        val HistoryScanState(evtsIn, identities, thingIds, prevState) = scanState
        val evts =
          if (evtsIn.size == maxRecords) {
            evtsIn.dequeue._2
          } else {
            evtsIn
          }
        val evt = historyEvt.evt
        val state = evolveState(Some(prevState))(evt)
        val (summary, newIdentities, newThingIds) =
          toSummary(evt, historyEvt.sequenceNr, identities, thingIds)(state)
        Future.successful(HistoryScanState(evts.enqueue(summary), newIdentities, newThingIds, state))
    }
  }

  /**
   * Run through the OIDs needed for the History, and get their Names. We have to do this in a
   * second pass because, in order to cope with Computed Names, this needs to be done with Future.
   */
  def mapNames(
    ids: Set[OID]
  )(implicit
    rc: RequestContext,
    state: SpaceState
  ): Future[ThingNames] = {
    (Future.successful(Map.empty[TID, String]) /: ids) { (fut, id) =>
      fut.flatMap { m =>
        state.anything(id) match {
          case Some(t) => t.nameOrComputed.map(name => m + (OID2TID(id) -> name))
          case _       => Future.successful(m)
        }
      }
    }
  }

  def getHistorySummary(
    end: Option[HistoryVersion],
    maxRecords: Int,
    rc: RequestContext
  ): RequestM[HistorySummary] = {
    val resultFut = for {
      HistoryScanState(evts, identityIds, thingIds, latestState) <- scanHistory(end, maxRecords)
      thingNames <- mapNames(thingIds)(rc, latestState)
      identities <- IdentityAccess.getIdentities(identityIds.toSeq)
      identityMap = identities.map {
        case (id, publicIdentity) =>
          (id.toThingId.toString -> ClientApi.identityInfo(publicIdentity))
      }
    } yield HistorySummary(evts, EvtContext(identityMap, thingNames))

    loopback(resultFut)
  }

}
