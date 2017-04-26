package funcakka

import cats._
import cats.implicits._
import cats.instances._

import akka.actor.Actor.Receive

/**
 * Abstraction of an ActorRef.
 * 
 * TODO: we don't yet really have a good representation of "?". That should be added before too long.
 * Note that it's a complex problem, though, since it is tied up with PersistentActorCore.monadError.
 * (That is, "?" shouldn't return Future, it should return ME.)
 */
trait ActorRefLike[T] {
  def !(t:T)(message:Any)(implicit sender:T):Unit
}
object ActorRefLike {
  class Ops[T : ActorRefLike](t:T) {
    def !(message:Any)(implicit sender:T):Unit = implicitly[ActorRefLike[T]].!(t)(message)
  }
}

/**
 * This trait abstracts the concept of a PersistentActor, so that the same code can be written against either
 * the real Actor or a synchronous test world.
 * 
 * Note that this is intentionally *not* a typeclass. Doing it that way turns out to be problematic, because
 * you have to concretely instantiate abstract methods like receiveRecover() from the base class. So it's just
 * not really worth trying to shove this into a typeclass.
 * 
 * There are two distinct implementations of this. RealActorCore gets mixed into your actor Actor, and provides
 * the glue to relate this to PersistentActor. TestActorCore, OTOH, is a synchronous test harness for unit testing
 * and introspecting on your core code.
 * 
 * You should implement a "Core" trait that extends PersistentActorCore, and implements persistenceId,
 * receiveCommand and receiveRecover. You then mix that into your real PersistentActor along with RealActorCore
 * and Requester.
 * 
 * TODO: refactor this stuff so that you can use it with Actor, PersistentActor and Requester separately!
 * 
 * TODO: this abstraction isn't complete yet -- it's just the parts that I have found myself needing in practice.
 * In the long run, it should be fleshed out more.
 */
trait PersistentActorCore {
  
  /**
   * The actual type of our MonadErrors.
   */
  type ME[T]
  implicit def monadError:MonadError[ME, Throwable]
  
  /**
   * This is a sad abstraction break that is necessary for the moment: there are a lot of functions
   * that explicitly return Futures, and it's going to take quite some time before we can eliminate those.
   * 
   * TODO: every usage of fromFuture() is inherently a code smell. Work on steadily eliminating them.
   */
  def fromFuture[T : scala.reflect.ClassTag](fut:scala.concurrent.Future[T]):ME[T]
  
  /**
   * The actual type for ActorRefs.
   */
  type AR
  implicit val actorRefLike:ActorRefLike[AR]
  
  implicit def sender:AR
  def self:AR
  
  /**
   * The ID of this PersistentActor. Your Core trait should usually implement this.
   */
  def persistenceId:String
  
  /**
   * Handle a Command from the outside. Your Core trait should usually implement this.
   */
  def receiveCommand:Receive
  
  /**
   * Handle a recovered Event from the Persistence stream. Your Core trait should usually implement this.
   */
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
   * Our own version of persist().
   * 
   * IMPORTANT: this is *not* just a simple shell around the build-in persist() function! This
   * is coded to return a proper Monad instead of using a callback, so that you can chain things
   * without tying yourself in knots.
   * 
   * The returned MonadError will resolve once persistence is complete. This is *not* guaranteed to
   * happen synchronously in the general case, but will do so if ME == RequestM, as in the normal
   * RealActorCore. (This signature is not recommended if ME == Future.)
   */
  def persistAnd[Evt](event:Evt):ME[Evt]
  
  /**
   * Our version of persistAll().
   * 
   * Note that the MonadError will resolve after *all* of the events have been persisted! This is
   * very different from the built-in callback on the native persistAll(), which calls the callback
   * for *each* event.
   */
  def persistAllAnd[Evt](events:collection.immutable.Seq[Evt]):ME[Seq[Evt]]
  
  /**
   * The sequence ID of the most recently-processed persistence message. Normally implemented by
   * PersistentActor.
   */
  def lastSequenceNr:Long
  
  /**
   * From PersistentActor, this saves the state of this Space as a whole, and will fire a
   * SaveSnapshotSuccess or SaveSnapshotFailure message.
   */
  def saveSnapshot(snapshot:Any):Unit
}
