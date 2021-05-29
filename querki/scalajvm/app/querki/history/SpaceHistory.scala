package querki.history

import scala.collection.immutable.{Queue, VectorBuilder}
import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import akka.persistence._
import akka.persistence.cassandra.query.scaladsl._
import akka.persistence.query._
import akka.stream.ActorMaterializer
import org.querki.requester._
import models._
import querki.data.TID
import querki.globals._
import querki.history.HistoryFunctions._
import querki.history.SpaceHistory._
import querki.identity.User
import querki.identity.IdentityPersistence.UserRef
import querki.spaces.{SpacePure, TracingSpace}
import querki.spaces.messages._
import querki.spaces.SpaceMessagePersistence._
import querki.time._
import querki.util.{PublicException, QuerkiActor, SingleRoutingParent, TimeoutChild}
import querki.values.{QLContext, QValue, RequestContext, SpaceState}
import HistoryFunctions._
import akka.persistence.query.scaladsl.{CurrentEventsByPersistenceIdQuery, ReadJournal}
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
     with TimeoutChild {
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Core = interface[querki.core.Core]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val QL = interface[querki.ql.QL]
  lazy val SystemState = interface[querki.system.System].State

  lazy val ExactlyOne = Core.ExactlyOne
  lazy val LinkType = Core.LinkType
  lazy val YesNoType = Core.YesNoType

  def timeoutConfig: String = "querki.history.timeout"

  def persistenceId = id.toThingId.toString

  lazy val tracing = TracingSpace(id, "SpaceHistory: ")

  // TODO: this is more than a little bit hacky. We should come up with a more principled approach to testing,
  // likely with machinery to override Ecology members. But it'll do for now.
  lazy val test =
    Config.getString("akka.persistence.journal.plugin") == "inmemory-journal"

  lazy val readJournal: ReadJournal with CurrentEventsByPersistenceIdQuery =
    if (test)
      // During tests, we are using the in-memory journal. Yes, this is the documented way to do it:
      //   https://github.com/dnvriend/akka-persistence-inmemory
      PersistenceQuery(context.system)
        .readJournalFor("inmemory-read-journal")
        .asInstanceOf[ReadJournal with CurrentEventsByPersistenceIdQuery]
    else
      PersistenceQuery(context.system).readJournalFor[CassandraReadJournal](CassandraReadJournal.Identifier)

  case class HistoryRecord(
    sequenceNr: Long,
    evt: SpaceEvent,
    state: SpaceState
  )
  type StateHistory = Vector[HistoryRecord]
  val StateHistory = Vector

  // The full history of this Space, *one*-indexed. (That seems to be the way Akka Persistence works.)
  // Be careful using this: the 1-indexing isn't at all obvious.
  // TODO: if we start to cope with extracting slices of the history, we'll need to maintain the
  // base index. Note that we can count on sequenceNr increasing monotonically (per discussion in
  // akka-persistence-cassandra Gitter, 9/13/16), so using a Vector makes sense here.
  // TODO: once we switch to fully dynamic, delete this.
  var history = StateHistory.empty[HistoryRecord]

  def latestState = history.lastOption.map(_.state).getOrElse(emptySpace)

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

  /**
   * This reads all of the history since the last time it was called. It is designed so that we can
   * reload the client page and get any new events since it was last shown.
   *
   * TODO: once everything has been switched to running dynamically, delete this.
   */
  def readCurrentHistory(): RequestM[Unit] = {
    tracing.trace("readCurrentHistory")
    // TODO: this is utterly *profligate* with RAM. Think about how we might want to restructure this in
    // order to not hold the entire thing in memory all the time, while still providing reasonably
    // responsive access. Should we instead rebuild stuff on-demand? Should the client signal what
    // range of events it is looking at, and we load those in?
    val source = readJournal.currentEventsByPersistenceId(persistenceId, history.size + 1, Int.MaxValue)
    implicit val mat = ActorMaterializer()
    val initialState =
      if (history.isEmpty)
        emptySpace
      else
        history.last.state
    loopback {
      // Note that we construct the history using VectorBuilder, for efficiency:
      source.runFold((initialState, new VectorBuilder[HistoryRecord])) {
        // Note that this quite intentionally rejects anything that isn't a SpaceEvent!
        case (((curState, history), EventEnvelope(offset, persistenceId, sequenceNr, evt: SpaceEvent))) => {
          val nextState = evolveState(Some(curState))(evt)
          history += HistoryRecord(sequenceNr, evt, nextState)
          (nextState, history)
        }
      }
    }.map { fullHist =>
      history = history ++ fullHist._2.result
      mat.shutdown()
    }
  }

  /**
   * The interesting bits from a single history record.
   *
   * Note that this is a subset of the underlying [[EventEnvelope]], mostly to hide the fields that I don't think we
   * should ever care about.
   */
  case class HistoryEvent(
    sequenceNr: Long,
    evt: SpaceEvent
  )

  /**
   * Run the given evolution operation over a range of history records.
   *
   * @param start the sequence number of the record to start with
   * @param end the sequence number to end at
   * @param zero the initial state of the fold
   * @param evolve the actual function to fold over
   * @tparam T the type of the fold
   * @return a Future of the result of the fold
   */
  def foldOverPartialHistory[T](
    start: Long,
    end: Long
  )(
    zero: T
  )(
    evolve: (T, HistoryEvent) => Future[T]
  ): Future[T] = {
    tracing.trace("foldOverHistory")
    val source = readJournal.currentEventsByPersistenceId(persistenceId, start, end)
    implicit val mat = ActorMaterializer()
    source.runFoldAsync(zero) {
      // Note that this quite intentionally rejects anything that isn't a SpaceEvent!
      case (current, EventEnvelope(offset, persistenceId, sequenceNr, evt: SpaceEvent)) => {
        evolve(current, HistoryEvent(sequenceNr, evt))
      }
      case (current, _) => Future.successful(current)
    }.map { result =>
      mat.shutdown()
      result
    }
  }

  /**
   * Run the given evolution operation over the entire history of this Space.
   *
   * Note that [[foldOverPartialHistory()]] is more general, but less often useful.
   *
   * The [[evolve]] function is async because we often need that. If the algorithm you need is synchronous, just
   * wrap the result in Future.successful().
   *
   * @param zero the initial state of the fold
   * @param evolve the actual function to fold over
   * @tparam T the type of the fold
   * @return a Future of the result of the fold
   */
  def foldOverHistory[T](zero: T)(evolve: (T, HistoryEvent) => Future[T]): Future[T] = {
    foldOverPartialHistory(1, Long.MaxValue)(zero)(evolve)
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

  // Copied from YesNoUtils
  // TODO: refactor YesNoUtils so that it doesn't require being mixed in with an Ecot. The challenge is
  // that the Ecot definition of Core isn't precisely the same as the definition here, because it is an
  // initRequires -- it *acts* the same, but it's a different type. We we need a concept of "a way to get
  // at Core".
  def toBoolean(typed: QValue): Boolean = {
    if (typed.pType == YesNoType)
      typed.firstAs(YesNoType).getOrElse(false)
    else
      false
  }

  def findAllDeleted(
    rc: RequestContext,
    predicateOpt: Option[QLExp],
    renderOpt: Option[QLExp]
  ): Future[List[QValue]] = {
    // This is a bit complex. Ultimately, we're building up a list of QValues, which are the rendered results
    // for each deleted item. But we need to track the OID for each of those, so that we can filter out duplicates.
    // And we need to keep track of the *previous* SpaceState before each deletion event, so that we can render
    // the deleted item in terms of that previous state.
    // Down at the bottom, we will throw away everything but the QValues.
    foldOverHistory((List.empty[(QValue, OID)], emptySpace)) { (v, record) =>
      val (soFar, prevState) = v
      val HistoryEvent(sequenceNr, evt) = record
      val state = evolveState(Some(prevState))(evt)
      evt match {
        case DHDeleteThing(req, thingId, modTime) => {
          // Evaluate the predicate on this thing in the context of the *previous* state, when the
          // Thing still existed:
          val endTime = ecology.api[querki.time.TimeProvider].qlEndTime
          val context = QLContext(ExactlyOne(LinkType(thingId)), Some(rc), endTime)(prevState, ecology)
          val predicateResultFut: Future[Boolean] = predicateOpt match {
            case Some(predicate) =>
              try {
                QL.processExp(context, predicate).map(_.value).map(toBoolean(_)).recover {
                  case ex: Exception => { ex.printStackTrace(); false }
                }
              } catch {
                case ex: Exception => Future.successful(false)
              }
            case _ => Future.successful(true)
          }
          predicateResultFut.flatMap { passes =>
            def justTheOID(): Future[(List[(QValue, OID)], SpaceState)] = {
              val deleted = (ExactlyOne(LinkType(thingId)), thingId)
              val filtered = soFar.filterNot(_._2 == thingId)
              Future.successful((deleted :: filtered, state))
            }

            if (passes) {
              // Passes the predicate, so figure out its name, and add it to the list:
              renderOpt.map { render =>
                prevState.anything(thingId).map { thing =>
                  QL.processExp(context, render).map { result =>
                    // Remove any *previous* deletions of this Thing -- we want to wind up with only the
                    // most recent. This is a little inefficient in Big-O terms, but I'm guessing that won't
                    // matter a lot in reality:
                    val deleted = (result.value, thingId)
                    val filtered = soFar.filterNot(_._2 == thingId)
                    (deleted :: filtered, state)
                  }.recoverWith {
                    case ex: Exception => {
                      ex.printStackTrace()
                      justTheOID()
                    }
                  }
                }.getOrElse {
                  // Oddly, we didn't find that OID in the previous state, so give up and show the raw OID:
                  justTheOID()
                }
              }.getOrElse {
                justTheOID()
              }
            } else {
              // Didn't pass the predicate, so just keep going:
              Future.successful((soFar, state))
            }
          }
        }

        // Anything other than deletions is ignored -- move along...
        case _ => Future.successful((soFar, state))
      }
    }
      .map { result =>
        val (deletedPairs, state) = result

        // Filter out all restored items
        val finalDeletedPairs =
          deletedPairs
            .filterNot { pair =>
              val (qv, oid) = pair
              state.anything(oid).isDefined
            }

        // Extract just the List of QValues from all of that:
        finalDeletedPairs.map(_._1)
      }
  }

  case class HistoryScanState(
    evts: Queue[EvtSummary],
    identityIds: Set[OID],
    thingsIds: Set[OID],
    state: SpaceState
  )

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

  def getHistorySummary(
    end: Option[HistoryVersion],
    maxRecords: Int,
    rc: RequestContext
  ): RequestM[HistorySummary] = {
    val resultFut = for {
      HistoryScanState(evts, identityIds, thingIds, state) <- scanHistory(end, maxRecords)
      thingNames <- mapNames(thingIds)(rc, latestState)
      identities <- IdentityAccess.getIdentities(identityIds.toSeq)
      identityMap = identities.map {
        case (id, publicIdentity) =>
          (id.toThingId.toString -> ClientApi.identityInfo(publicIdentity))
      }
    } yield HistorySummary(evts, EvtContext(identityMap, thingNames))

    loopback(resultFut)
  }

  def doReceive = {
    case GetHistorySummary(rc) => {
      tracing.trace(s"GetHistorySummary")
      // TODO: get the end and maxRecords from the front end:
      getHistorySummary(None, 1000, rc).map { summary =>
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

    case RestoreDeletedThing(user, thingId) => {
      tracing.trace(s"RestoreDeletedThing($thingId)")
      readCurrentHistory().map { _ =>
        // Sanity-check: don't try to recreate the Thing if it currently exists!
        history.last.state.anything(thingId) match {
          case Some(thing) => {
            // We don't need to do anything, because it's not currently deleted
            sender ! ThingFound(thingId, history.last.state)
          }

          case None => {
            // The normal case: it's not there
            // Seek backwards through the History until we find this Thing.
            // There might be an abstraction fighting to break out here: keep an eye open for
            // other functions that want this "look backwards until" behavior.
            @annotation.tailrec
            def findThingRec(index: Int): Option[Thing] = {
              if (index < 0)
                None
              else
                history(index).state.anything(thingId) match {
                  case Some(thing) => Some(thing)
                  case None        => findThingRec(index - 1)
                }
            }

            findThingRec(history.size - 1) match {
              case Some(thing) => {
                val createMsg =
                  CreateThing(
                    user,
                    id,
                    thing.kind,
                    thing.model,
                    thing.props,
                    Some(thingId),
                    creatorOpt = thing.creatorOpt,
                    createTimeOpt = thing.createTimeOpt
                  )
                spaceRouter.request(createMsg).foreach {
                  case resp: ThingFound => sender ! resp
                  case other =>
                    throw new Exception(s"RestoreDeletedThing, in Space $id, for Thing $thingId, got response $other!")
                }
              }
              case None => throw new Exception(s"RestoreDeletedThing couldn't find Thing $thingId!")
            }
          }
        }
      }
    }

    case GetCurrentState(rc) => {
      // This is only legal for admins:
      if (rc.requesterOrAnon.isAdmin) {
        readCurrentHistory().map { _ =>
          sender ! CurrentState(latestState)
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
  case class GetHistorySummary(rc: RequestContext) extends HistoryMessage

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
    thingId: OID
  ) extends HistoryMessage

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
