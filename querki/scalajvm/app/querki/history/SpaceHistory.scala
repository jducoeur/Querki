package querki.history

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import org.querki.requester._
import models._
import querki.globals._
import querki.history.SpaceHistory._
import querki.identity.User
import querki.identity.IdentityPersistence.UserRef
import querki.spaces.{SpacePure, TracingSpace}
import querki.spaces.messages._
import querki.spaces.SpaceMessagePersistence._
import querki.util.{PublicException, QuerkiActor, SingleRoutingParent, TimeoutChild}
import querki.values.{QValue, RequestContext, SpaceState}
import HistoryFunctions._
import querki.ql.QLExp

/**
 * This is a very simplistic wrapper around SpaceHistory, so that the latter can only be in
 * memory when needed. It routes all messages to SpaceHistory.
 *
 * This lives under SpaceRouter, as part of the standard troupe.
 */
class SpaceHistoryParent(
  e: Ecology,
  val id: OID,
  val spaceRouter: ActorRef
) extends Actor
     with ReceivePipeline
     with SingleRoutingParent {
  def createChild(): ActorRef = context.actorOf(Props(classOf[SpaceHistory], e, id, spaceRouter))

  def receive = {
    case msg => routeToChild(msg)
  }
}

/**
 * This is essentially a variant of the PersistentSpaceActor, which reads in the complete history
 * of a Space, and provides access to it.
 */
private[history] class SpaceHistory(
  e: Ecology,
  val id: OID,
  val spaceRouter: ActorRef
) extends QuerkiActor(e)
     with SpacePure
     with ModelPersistence
     with ReceivePipeline
     with TimeoutChild
     with HistoryFoldingImpl
     with RestoreDeleted
     with HistorySummaryBuilder
     with FindDeleted {

  def timeoutConfig: String = "querki.history.timeout"

  def persistenceId = id.toThingId.toString

  lazy val tracing = TracingSpace(id, "SpaceHistory: ")

  case class HistoryRecord(
    sequenceNr: Long,
    evt: SpaceEvent,
    state: SpaceState
  )

  def getHistoryRecord(v: HistoryVersion): RequestM[HistoryRecord] = {
    val resultFut = foldOverPartialHistory(1, v)(Option.empty[HistoryRecord]) { (prevOpt, historyEvt) =>
      val prevState = prevOpt.map(_.state).getOrElse(emptySpace)
      val state = evolveState(Some(prevState))(historyEvt.evt)
      Future.successful(Some(HistoryRecord(historyEvt.sequenceNr, historyEvt.evt, state)))
    }

    loopback(resultFut.map(_.getOrElse {
      // This is rather weird -- we didn't get any events:
      HistoryRecord(0, DHInitState(UserRef(User.Anonymous.id, None), ""), emptySpace)
    }))
  }

  def getCurrentState(): RequestM[SpaceState] = {
    val resultFut =
      foldOverHistory(emptySpace) { (prevState, historyEvt) =>
        Future.successful(evolveState(Some(prevState))(historyEvt.evt))
      }

    loopback(resultFut)
  }

  def doReceive = {
    case GetHistorySummary(rc, end, nRecords) => {
      tracing.trace(s"GetHistorySummary")
      getHistorySummary(end, nRecords, rc).map { summary =>
        sender ! summary
      }
    }

    case GetHistoryVersion(v) => {
      tracing.trace("GetHistoryVersion")
      getHistoryRecord(v).map { record =>
        sender ! CurrentState(record.state)
      }
    }

    case RollbackTo(v, user) => {
      tracing.trace(s"RollbackTo($v)")
      getHistoryRecord(v).map { record =>
        spaceRouter.request(SetState(user, id, record.state, SetStateReason.RolledBack, v.toString)).foreach {
          _ match {
            case resp: ThingFound => sender ! resp
            case other =>
              throw new Exception(s"Tried to roll space $id back to version $v, but received response $other")
          }
        }
      }
    }

    case RestoreDeletedThing(user, thingIds) => {
      tracing.trace(s"RestoreDeletedThing($thingIds)")
      restoreDeletedThings(user, thingIds.toSet).map { resultingState =>
        sender ! Restored(thingIds, resultingState)
      }
    }

    case GetCurrentState(rc) => {
      // This is only legal for admins:
      if (rc.requesterOrAnon.isAdmin) {
        getCurrentState().map { curState =>
          sender ! CurrentState(curState)
        }
      } else {
        sender ! SpaceBlocked(PublicException("Space.blocked"))
      }
    }

    case ForAllDeletedThings(rc, predicateOpt, renderOpt) => {
      loopback(findAllDeleted(rc, predicateOpt, renderOpt)).map { deletedList =>
        sender ! DeletedThings(deletedList)
      }.recover {
        case ex: Exception => spew(s"Got an Exception: $ex")
      }
    }
  }
}

object SpaceHistory {

  def actorProps(
    e: Ecology,
    id: OID,
    spaceRouter: ActorRef
  ) = Props(classOf[SpaceHistoryParent], e, id, spaceRouter)

  /**
   * Note that HistoryMessages *can* be used as a SpaceMessagePayload, but mostly are routed directly.
   * TODO: we might want to change that, for consistency. Is there a reason for this to be a special
   * snowflake?
   */
  sealed trait HistoryMessage extends SpaceMessagePayload

  /**
   * Fetches the HistorySummary (as described in the public API).
   */
  case class GetHistorySummary(
    rc: RequestContext,
    end: Option[HistoryVersion],
    nRecords: Int
  ) extends HistoryMessage

  /**
   * Fetch a specific version of the history of this Space. Returns a CurrentState().
   */
  case class GetHistoryVersion(v: HistoryVersion) extends HistoryMessage

  /**
   * Resets the current state of this Space to the specified Version.
   *
   * The caller is expected to have already done confirmation! This doesn't precisely lose
   * information, but can hide a lot!
   */
  case class RollbackTo(
    v: HistoryVersion,
    user: User
  ) extends HistoryMessage

  /**
   * Finds the last existing revision of the specified Thing, and re-creates it in this Space.
   */
  case class RestoreDeletedThing(
    user: User,
    thingIds: Seq[OID]
  ) extends HistoryMessage

  /**
   * Response from [[RestoreDeletedThing]].
   */
  case class Restored(
    thingIds: Seq[OID],
    state: SpaceState
  )

  /**
   * Return the current State.
   *
   * This is rarely used in reality -- normally, it would make more sense to get this from the real SpaceActor. This
   * is a backdoor that allows you to fetch the State without actually loading it completely. It is intended for
   * admin use only, mainly for forensics.
   */
  case class GetCurrentState(rc: RequestContext) extends HistoryMessage

  /**
   * Looks for all of the deleted Things in the Space that match the given QL predicate at the time when they
   * were deleted. Returns them as a [[DeletedThings]] message.
   *
   * @param predicate a QL expression that should return YesOrNo; if missing, all deleted Things are returned
   */
  case class ForAllDeletedThings(
    rc: RequestContext,
    predicate: Option[QLExp],
    render: Option[QLExp]
  ) extends HistoryMessage

  case class DeletedThings(things: List[QValue])
}
