package querki.persistence

import scala.util.Success

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
  
  def doPersistAll(events:collection.immutable.Seq[UseKryo])(handler: UseKryo => Unit):Unit
  
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

/**
 * Common code for PersistentActorCores that use the RM abstraction.
 * 
 * TODO: this whole thing is pretty crappy. In retrospect, the RM/RTC thing should have been handled with a
 * typeclass. When we get a chance, rewrite it as such.
 */
trait PersistentRMCore[RM[_]] { self:PersistentActorCore =>
  def rtc:querki.spaces.RTCAble[RM]
  
  /**
   * This is a bit subtle, but turns out abstract RM into a RequestTC, which has useful operations on it.
   */
  implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
    
  /**
   * A wrapper around persist() that allows us to chain from it. No clue why this isn't built into Akka Persistence.
   */
  def persistAllAnd(events:collection.immutable.Seq[UseKryo]):RM[Seq[UseKryo]] = {
    val rm = rtc.prep[Seq[UseKryo]]
    doPersistAll(events) { _ =>
      rm.resolve(Success(events))
    }
    rm
  }
  
  def persistAnd[A <: UseKryo](event:A)(handler: (A) => Unit):RM[A] = {
    val rm = rtc.prep[A]
    doPersist(event) { _ =>
      rm.resolve(Success(event))
    }
    rm
  }
}
