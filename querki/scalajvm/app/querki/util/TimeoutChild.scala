package querki.util

import scala.collection.immutable.Queue
import scala.concurrent.duration._

import akka.actor._
import akka.contrib.pattern.ReceivePipeline
import ReceivePipeline._

import querki.globals._

/**
 * The child of a RoutingParent, which has a built-in inactivity timeout. Concrete classes must
 * also extend ReceivePipeline. Note that ReceivePipeline must be extended *after* PersistentActor!
 * 
 * Note that ClusterTimeoutChild is quite similar, but designed specifically to work with
 * ClusterSharding.
 */
trait TimeoutChild extends Actor { pipe:ReceivePipeline =>
  
  /**
   * Instances must define this -- it is the name of the config string that defines how long
   * the timeout interval is.
   */
  def timeoutConfig:String
  
  override def preStart() = {
    val timeout = context.system.settings.config.getDuration(timeoutConfig, java.util.concurrent.TimeUnit.MILLISECONDS)
    context.setReceiveTimeout(Duration(timeout, MILLISECONDS))
    super.preStart()
  }
  
  pipelineInner {
    case resp:ReceiveTimeout => {
      context.parent ! KillMe
      HandledCompletely
    }
  }
}

object RoutingStates {
  val StateNormal = 1
  val StateDying = 2
}

/**
 * This encapsulates the concept of a parent that has TimeoutChildren. You don't use this directly,
 * you use its subtraits, depending on how many children you're looking for.
 */
trait RoutingParentBase[K] extends Actor { pipe:ReceivePipeline =>

  class ManagedChild(val id:K, val ref:ActorRef) {
    import RoutingStates._
    var state = StateNormal
    var buffer = Queue.empty[Any]
    
    def beginShutdown = {
      state = StateDying
      ref ! PoisonPill
    }
    
    def forward(msg:Any) = {
      state match {
        case StateNormal => ref.forward(msg)
        case StateDying => buffer.enqueue(msg)
      }
    }
  }
  
  def findChild(ref:ActorRef):Option[ManagedChild]
  
  /**
   * Instances of RoutingParent must implement this. Given the child's key, create it.
   */
  def createChild(key:K):ActorRef
  
  def childrenUpdated() = {}
  
  /**
   * Instances of RoutingParent *may* override this. It is called immediately after creating a
   * new Child.
   */
  def initChild(child:ActorRef) = {}
  
  protected def createManagedChild(key:K):ManagedChild = {
    val c = new ManagedChild(key, createChild(key))
    context.watch(c.ref)
    initChild(c.ref)
    childrenUpdated()
    c    
  }
  
  def routeToChild(key:K, msg:Any)
  def removeChild(key:K)
  
  pipelineInner {
    case KillMe => {
      findChild(sender) match {
        case Some(child) => child.beginShutdown
        case _ => QLog.warn(s"RouterParent got KillMe from unknown child $sender")
      }
      HandledCompletely
    }
    
    case msg @ Terminated(other) => {
      findChild(other) match {
        case Some(child) => {
          // Okay, the child is now completely finished...
          val key = child.id
          removeChild(key)
          if (!child.buffer.isEmpty) {
            // ... but more messages came in the meantime, so rebuild the child:
            child.buffer.foreach(msg => routeToChild(key, msg))
          }
          childrenUpdated()
          HandledCompletely
        }
        // It's a Terminated from something else?
        case None => Inner(msg)
      }
    }
  }
}

/**
 * This is a simple parent, that may have *one* optional child. This is useful when the
 * child's state is big and complicated, and we don't want to keep it in memory all the
 * time.
 * 
 * TODO: we should figure out a way to parameterize this so that a single parent can have
 * multiple distinct children of different types. I suspect that requires a rethink to
 * be typeclass-based.
 */
trait SingleRoutingParent extends RoutingParentBase[Unit] { pipe:ReceivePipeline =>
  private var _child:Option[ManagedChild] = None
  
  def findChild(ref:ActorRef):Option[ManagedChild] = {
    if (_child.map(_.ref.path.name == ref.path.name).getOrElse(false))
      _child
    else
      None
  }
  
  def createChild():ActorRef
  def createChild(key:Unit):ActorRef = createChild()
  
  def routeToChild(key:Unit, msg:Any):Unit = {
    val child = _child match {
      case Some(c) => c
      case None => createManagedChild(())
    }
    child.forward(msg)
  }
  def routeToChild(msg:Any):Unit = routeToChild((), msg)
  
  def removeChild(key:Unit) = _child = None
}

/**
 * This trait encapsulates the idea of a Parent/Router that owns a bunch of distinct children,
 * each of which has a clear ID. When it receives a message for a child, it routes it there,
 * creating the child if it doesn't already exist.
 * 
 * Concrete classes must also extend ReceivePipeline.
 * 
 * As currently constructed, a given Actor can only be the RoutingParent for a single kind of
 * child.
 * 
 * Yes, this is quite similar to Cluster Sharding. But it is local-only, lighter-weight, and
 * has timeout built in. We generally use it *under* our top-level Cluster Sharded entities,
 * to manage particular Troupes.
 */
trait RoutingParent[K] extends RoutingParentBase[K] { pipe:ReceivePipeline =>
  
  /**
   * All of the current children we are routing to.
   */
  private var _children = Map.empty[K, ManagedChild]
  
  def findChild(ref:ActorRef):Option[ManagedChild] = _children.find(_._2.ref.path.name == ref.path.name).map(_._2)
  
  def children = _children.values.map(_.ref)
  def child(id:K) = _children.get(id).map(_.ref)
  def nChildren = _children.size
  def removeChild(key:K) = _children = _children - key
  
  override protected def createManagedChild(key:K):ManagedChild = {
    val c = super.createManagedChild(key)
    _children = _children + (key -> c)
    c    
  }
  
  /**
   * This should be called inside the receive() clause for a message that is intended for a child.
   * It will deal with creating the child if necessary.
   */
  def routeToChild(key:K, msg:Any) = {
    val child = _children.get(key) match {
      case Some(c) => c
      case None => createManagedChild(key)
    }
    child.forward(msg)
  }
  
  /**
   * This should be called inside the receive() clause for a message; it forwards this message
   * to *all* currently-active children.
   */
  def routeToAll(msg:Any) = children.foreach { _.forward(msg) }
}
