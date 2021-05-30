package querki.history

import akka.persistence.query.EventEnvelope
import querki.globals.Future
import querki.spaces.SpaceMessagePersistence.SpaceEvent
import querki.spaces.SpacePure

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
