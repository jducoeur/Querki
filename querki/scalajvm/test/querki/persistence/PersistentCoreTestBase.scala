package querki.persistence

import scala.collection.immutable.Queue

import akka.actor.Actor.Receive
import akka.persistence._

import querki.time.DateTime

case class HistoryRecord(sequenceNr:Long, msg:Any)

/**
 * This provides a consistent implementation of the key functions of PersistentActor, so that we
 * can easily write synchronous unit tests for the various Core classes.
 */
trait PersistentCoreTestBase extends PersistentActorCore {
  // These should be defined in the constructor of the actual test class
  def initHistory:List[HistoryRecord]
  
  // These methods from PersistentActor should be implemented by the *real* class
  def persistenceId:String
  def receiveCommand:Receive  
  def receiveRecover:Receive
  
  /**
   * We don't currently expect this to be called during tests, although that might change.
   */
  def handleRequestResponse:Receive = ???
  
  /**
   * Simple emulator for stash(), if necessary.
   */
  def stash():Unit = {
    _stash = _stash :+ _curMsg.get
  }
  def unstashAll():Unit = {
    _stash.map { msg =>
      receiveCommand(msg)
    }
    _stash = Queue.empty
  }
  var _stash:Queue[AnyRef] = Queue.empty
  
  /**
   * This is the "history" of "persisted" events, in reverse chronological order. (That is, most recent is
   * at the front.)
   */
  var history = initHistory
  
  def doPersist[A <: UseKryo](event:A)(handler: (A) => Unit) = {
    lastSequenceNr += 1
    history = HistoryRecord(lastSequenceNr, event) :: history
    handler(event)
  }
  
  var lastSequenceNr:Long = 0
  
  /**
   * This sends the given message back to sender.
   */
  def respond(msg:AnyRef) = {
    currentResponses = msg :: currentResponses
  }
  
  /**
   * The responses to the current message.
   */
  var currentResponses:List[AnyRef] = List.empty
  
  /**
   * The message currently being processed, if any. Needed for stash().
   */
  var _curMsg:Option[AnyRef] = None
  
  /**
   * Called by the test code. Returns the most recent response, if there were any.
   */
  def aroundReceive(msg:AnyRef):Option[AnyRef] = {
    currentResponses = List.empty
    _curMsg = Some(msg)
    try {
      receiveCommand(msg)
    } finally {
      _curMsg = None
    }
    currentResponses.headOption
  }
  
  def saveSnapshot(snapshot:Any) = {
    val metadata = SnapshotMetadata(persistenceId, lastSequenceNr, DateTime.now.getMillis)
    val event = SnapshotOffer(metadata, snapshot)
    // Note that the snapshot *replaces* the rest of the history, intentionally. Playback should start
    // from here:
    history = HistoryRecord(lastSequenceNr, event) :: Nil
    receiveCommand(SaveSnapshotSuccess(metadata))
  }
  
  /**
   * If an initial history was provided, that's effectively the persistence log, so play it
   * before we do anything else.
   */
  if (!initHistory.isEmpty) {
    // Reverse it to get chrono order:
    val playHistory = initHistory.reverse
    playHistory.foreach { record =>
      lastSequenceNr = record.sequenceNr
      receiveRecover(record.msg)
    }
  }
  receiveRecover(RecoveryCompleted)
  
}