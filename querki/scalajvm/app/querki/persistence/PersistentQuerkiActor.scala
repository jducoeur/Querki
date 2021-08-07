package querki.persistence

import akka.persistence._

import org.querki.requester._

/**
 * This is the base class that mixes our PersistentActorCore with the reality of
 * PersistentActor. It fills in the adapter functions that we always want.
 */
trait PersistentQuerkiActor extends PersistentActorCore with PersistentActor with Requester {

  /**
   * Our own version of persist(). Note that this enforces UseKryo at the signature level, so we
   * should use it instead of ordinary persist().
   *
   * This is abstract, implemented differently in the real system vs. test. IMPORTANT: in test, the
   * handler is called synchronously, whereas in the real code it is called asynchronously! The
   * guarantees of Akka Persistence state that no further messages will be processed until after
   * the handler is called, but that processing will happen *after* this returns!
   */
  def doPersist[A <: UseKryo](event: A)(handler: (A) => Unit) = {
    persist(event) { evt =>
      handler(evt)
    }
  }

  /**
   * Our version of persistAll(), which enforces UseKryo.
   */
  def doPersistAll(events: collection.immutable.Seq[UseKryo])(handler: UseKryo => Unit) = {
    persistAll(events) { evt =>
      handler(evt)
    }
  }

  /**
   * Encapsulates "sender !" in something a bit more unit-test-friendly. Obviously, this may only
   * be called inside the receiveCommand loop, or inside the guts of doPersist().
   */
  def respond(msg: AnyRef) = { sender ! msg }

}
