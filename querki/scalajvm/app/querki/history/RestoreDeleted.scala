package querki.history

import akka.actor.ActorRef
import querki.spaces.messages.{CreateThing, ThingFound}
import querki.values.SpaceState
import querki.globals._
import querki.spaces.SpaceMessagePersistence.DHDeleteThing
import org.querki.requester.{RequestM, Requester}
import querki.identity.User
import models.Thing

trait RestoreDeleted extends HistoryFolding with Requester {

  def spaceRouter: ActorRef

  case class RestoreScanState(
    prevState: SpaceState,
    lastKnownVersions: Map[OID, Thing]
  )

  /**
   * Scan through the history, and find the last version of each specified Thing before it was deleted.
   */
  def findLastKnownVersions(thingIds: Set[OID]): Future[Map[OID, Thing]] = {
    foldOverHistory(RestoreScanState(emptySpace, Map.empty)) { (scanState, historyEvt) =>
      val lastKnownVersions =
        historyEvt.evt match {
          case DHDeleteThing(req, id, modTime) if (thingIds.contains(id)) => {
            // This is one of the Things we want to restore, so grab its previous state:
            scanState.prevState.anything(id) match {
              case Some(thing) => scanState.lastKnownVersions + (id -> thing)
              case None        => scanState.lastKnownVersions
            }
          }
          case _ => scanState.lastKnownVersions
        }
      val state = evolveState(Some(scanState.prevState))(historyEvt.evt)
      Future.successful(RestoreScanState(state, lastKnownVersions))
    }.map(_.lastKnownVersions)
  }

  def restoreOneFromLastKnown(
    user: User,
    thing: Thing
  ): RequestM[ThingFound] = {
    val createMsg =
      CreateThing(
        user,
        id,
        thing.kind,
        thing.model,
        thing.props,
        Some(thing.id),
        creatorOpt = thing.creatorOpt,
        createTimeOpt = thing.createTimeOpt
      )

    spaceRouter.request(createMsg).map {
      case found: ThingFound => found
      case other => {
        val msg = s"Got $other when trying to recreate thing $thing"
        QLog.error(msg)
        throw new Exception(msg)
      }
    }
  }

  def restoreFromLastKnowns(
    user: User,
    lastKnownVersions: Map[OID, Thing]
  ): RequestM[SpaceState] = {
    lastKnownVersions.foldLeft(RequestM.successful(emptySpace)) { (prevReq, pair) =>
      val (oid, thing) = pair
      prevReq.flatMap(_ => restoreOneFromLastKnown(user, thing).map(_.state))
    }
  }

  def restoreDeletedThings(
    user: User,
    thingIds: Set[OID]
  ): RequestM[SpaceState] = {
    for {
      lastKnownVersions <- loopback(findLastKnownVersions(thingIds))
      resultingState <- restoreFromLastKnowns(user, lastKnownVersions)
    } yield resultingState
  }

}
