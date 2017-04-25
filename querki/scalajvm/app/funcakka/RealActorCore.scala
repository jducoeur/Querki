package funcakka

import scala.util.Success

import cats._
import cats.instances._

import akka.actor.ActorRef
import akka.persistence.PersistentActor
import org.querki.requester.{Requester, RequestM}

object RealActorRefLike {
  implicit val RERLInstance = new ActorRefLike[ActorRef] {
    def !(t:ActorRef)(message:Any)(implicit sender:ActorRef):Unit = {
      t ! message
    }
  }
}

import RealActorRefLike._

/**
 * This mix-in adapts PersistentActorCore to the reality of a PersistentActor.
 * 
 * Note that most of the methods in PersistentActorCore are not defined, but are
 * simply delegated onward to the final PersistentActor.
 */
trait RealActorCore extends PersistentActorCore { actor:PersistentActor with Requester =>
  
  type ME[T] = RequestM[T]
  val monadError = RequestMInstances.catsStdInstancesForRequestM
  
  def fromFuture[T : scala.reflect.ClassTag](fut:scala.concurrent.Future[T]):RequestM[T] = {
    loopback(fut)
  }
  
  type AR = ActorRef
  val actorRefLike = RealActorRefLike.RERLInstance
  
  /**
   * The main implementation of doPersist().
   * 
   * TODO: in principle, this really belongs up in PersistentActorCore. The reason we can't yet do that
   * is that MonadError has no equivalent of RequestM's prep() and resolve(). (And similarly, it doesn't
   * have anything similar to Promise and complete().) Can we come up with a typeclass that encapsulates this notion,
   * so we can abstract over it? Is it even possible to do a pure/synchronous version of that abstraction over
   * Either, for unit-testing? (Maybe not without cheating and involving a mutable var.)
   */
  def persistAnd[Evt](event:Evt):RequestM[Evt] = {
    val rm = RequestM.prep[Evt]
    persist(event) { persisted =>
      rm.resolve(Success(persisted))
    }
    rm
  }
  
  def persistAllAnd[Evt](events:collection.immutable.Seq[Evt]):RequestM[Seq[Evt]] = {
    val rm = RequestM.prep[Seq[Evt]]
    persistAll(events) { persisted =>
      // Note that this is called for *each* persisted...
    }
    // ... so we need to use deferAsync() to get called after *all* of them are persisted:
    deferAsync(events) { evts =>
      rm.resolve(Success(evts))      
    }
    rm
  }
}
