package querki.persistence

import akka.actor.Actor.Receive

/**
 * This is a base trait that abstracts PersistentActor, so that we can write synchronous unit tests
 * for the various "Core" classes.
 */
trait PersistentActorCore {
  
  def persistenceId:String
  
  def receiveCommand:Receive
  
  def receiveRecover:Receive
  
  /**
   * The standard Requester function.
   */
  def handleRequestResponse:Receive
  
  /**
   * The usual function, inherited from PersistentActor.
   */
  def stash():Unit
  
  def unstashAll():Unit

  /**
   * Our own version of persist(). Note that this enforces UseKryo at the signature level, so we
   * should use it instead of ordinary persist().
   * 
   * This is abstract, implemented differently in the real system vs. test. IMPORTANT: in test, the
   * handler is called synchronously, whereas in the real code it is called asynchronously! The
   * guarantees of Akka Persistence state that no further messages will be processed until after
   * the handler is called, but that processing will happen *after* this returns!
   */
  def doPersist[A <: UseKryo](event:A)(handler: (A) => Unit):Unit
  
  /**
   * The sequence ID of the most recently-processed persistence message. Normally implemented by
   * PersistentActor.
   */
  def lastSequenceNr:Long
  
  /**
   * Encapsulates "sender !" in something a bit more unit-test-friendly.
   */
  def respond(msg:AnyRef):Unit
  
  /**
   * From PersistentActor, this saves the state of this Space as a whole, and will fire a
   * SaveSnapshotSuccess or SaveSnapshotFailure message.
   */
  def saveSnapshot(snapshot:Any):Unit
  
}