package querki.history

import akka.actor.Actor
import akka.persistence.cassandra.query.scaladsl.CassandraReadJournal
import akka.persistence.query.scaladsl.{CurrentEventsByPersistenceIdQuery, ReadJournal}
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import akka.stream.ActorMaterializer
import querki.globals._
import querki.spaces.SpaceMessagePersistence.SpaceEvent
import querki.spaces.{SpacePure, TracingSpace}

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
 * The abstract trait that represents the ability to fold over a Space's History.
 *
 * This is the underlying functionality behind most History functions; it is lifted out here so that we can
 * refactor those functions.
 */
trait HistoryFolding extends SpacePure {

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
  ): Future[T]

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
  def foldOverHistory[T](zero: T)(evolve: (T, HistoryEvent) => Future[T]): Future[T]
}

trait HistoryFoldingImpl extends Actor with HistoryFolding {

  def tracing: TracingSpace
  def persistenceId: String

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

}
