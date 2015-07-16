package querki.util

import scala.collection.immutable.Queue
import scala.concurrent.duration._

import akka.actor._

/**
 * The child of a RoutingParent, which has a built-in inactivity timeout.
 * 
 * Note that ClusterTimeoutChild is quite similar, but designed specifically to work with
 * ClusterSharding.
 */
trait TimeoutChild extends Actor {
  
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
  
  override def unhandled(message: Any): Unit = {
    message match {
      case resp:ReceiveTimeout => context.parent ! KillMe
      case other => super.unhandled(other)
    }
  }
}

object RoutingStates {
  val StateNormal = 1
  val StateDying = 2
}

/**
 * This trait encapsulates the idea of a Parent/Router that owns a bunch of distinct children,
 * each of which has a clear ID. When it receives a message for a child, it routes it there,
 * creating the child if it doesn't already exist.
 * 
 * As currently constructed, a given Actor can only be the RoutingParent for a single kind of
 * child.
 */
trait RoutingParent[K] extends Actor {

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
  
  /**
   * All of the current children we are routing to.
   */
  private var _children = Map.empty[K, ManagedChild]
  
  def findChild(ref:ActorRef):Option[ManagedChild] = _children.find(_._2.ref.path.name == ref.path.name).map(_._2)
  
  def children = _children.values.map(_.ref)
  def child(id:K) = _children.get(id).map(_.ref)
  def nChildren = _children.size
  def childrenUpdated() = {}
  
  /**
   * Instances of RoutingParent must implement this. Given the child's key, create it.
   */
  def createChild(key:K):ActorRef
  
  /**
   * Instances of RoutingParent *may* override this. It is called immediately after creating a
   * new Child.
   */
  def initChild(child:ActorRef) = {}
  
  private def createManagedChild(key:K):ManagedChild = {
    val c = new ManagedChild(key, createChild(key))
    _children = _children + (key -> c)
    context.watch(c.ref)
    initChild(c.ref)
    childrenUpdated()
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
  
  override def unhandled(message: Any): Unit = {
    message match {
      case KillMe => {
        findChild(sender) match {
          case Some(child) => child.beginShutdown
          case _ => {}
        }
      }
      case Terminated(other) => {
        findChild(sender) match {
          case Some(child) => {
            // Okay, the child is now completely finished...
            val key = child.id
            _children = _children - key
            if (!child.buffer.isEmpty) {
              // ... but more messages came in the meantime, so rebuild the child:
              child.buffer.foreach(msg => routeToChild(key, msg))
            }
            childrenUpdated()
          }
          case None => // Not our problem
        }
      }
      case other => super.unhandled(other)
    }
  }
}
