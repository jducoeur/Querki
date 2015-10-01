package querki.cluster

import akka.actor._
import akka.pattern._
import akka.persistence._

import models.OID

import querki.globals._

/**
 * Actor that actually doles OIDs out on request. There should be exactly one of these on
 * each Querki node. Each one is (externally) assigned a Shard ID, and represents the current
 * persistent state of that Shard.
 * 
 * This mechanism is intentionally a bit inefficient: in order to save DB roundtrips and improve
 * average latency of OID allocation requests, the Actor "requests" OIDs in blocks. (Where a
 * "request" is basically persisting that we have reserved that many more OIDs.) We do *not*
 * persist each OID allocation, just these block allocations. This means that, during recovery,
 * we need to be conservative, and assume that the entire block has been allocated even if only
 * one OID actually has been. The result is that the OID namespace is *slightly* sparse; the theory
 * is that this is a reasonable price to pay for improved latency, but that deserves to be tested.
 * 
 * @author jducoeur
 */
class OIDAllocator(shardId:ShardId) extends PersistentActor {
  import OIDAllocator._
  
  override def persistenceId = s"alloc$shardId"
  
  lazy val allocBlockSize = Config.getInt("querki.cluster.allocBlockSize", 10)
  lazy val allocSnapshotInterval = Config.getInt("querki.cluster.allocSnapshotInterval", 10)
  lazy val shardFullBufferSize = Config.getInt("querki.cluster.shardFullBufferSize", 100)
  // When we hit the shardFullMark, raise the alarm that we need to shut down this shard:
  lazy val shardFullMark = Int.MaxValue - shardFullBufferSize
  
  var current:Int = 0
  var availableThrough:Int = 0
  var snapshotCount:Int = 0
  
  def countToSnapshot() = {
    snapshotCount += 1
    if (snapshotCount >= allocSnapshotInterval) {
      snapshotCount = 0
      saveSnapshot(AllocState(availableThrough))
    }
  }
  
  val receiveRecover:Receive = {
    case Alloc(blockSize) => {
      availableThrough += blockSize
      current = availableThrough
      countToSnapshot()
    }
    
    case SnapshotOffer(metadata, AllocState(a)) => {
      availableThrough = a
      current = availableThrough
    }
    
    case other => QLog.error(s"OIDAllocator got unexpected message $other")
  }
  
  def sendFull(n:Int):Unit = {
    if (n > 3)
      // Give up on shutting down cleanly, and just die...
      throw new Exception(s"OIDAllocator $shardId unable to send ShardFull! Dying...")
    
    context.parent.ask(QuerkiNodeCoordinator.ShardFull(shardId))(defaultTimeout) map {
      case Shutdown => context.stop(self)
      case _ => sendFull(n + 1)
    }    
  }
  
  val receiveCommand:Receive = {
    case NextOID => {
      def giveOID() = {
        if (current == Int.MaxValue) {
          // Emergency! At this point we just need to fall over.
          context.parent ! QuerkiNodeCoordinator.ShardFull(shardId)
          throw new Exception(s"Overfull OIDAllocator $shardId")
        }
        
        val oid = OID(shardId, current)
        sender ! NewOID(oid)        
        current += 1
        
        if (current == shardFullMark) {
        }
      }
      
      if (current >= availableThrough) {
        // We've used up the current allocation, so allocate more:
        persist(Alloc(allocBlockSize)) { msg =>
          availableThrough += allocBlockSize
          giveOID()
          countToSnapshot()
        }
      } else {
        // We're still in the current block:
        giveOID()
      }
    }
  }
}

object OIDAllocator {
  def actorProps(shardId:ShardId) = Props(classOf[OIDAllocator], shardId)
  
  /////////////////////////////////
  //
  // Public API
  //
  
  /**
   * Request the next available OID. Returns a NewOID.
   */
  case object NextOID
  
  /**
   * A newly-created, unique OID for this system to use.
   */
  case class NewOID(oid:OID)
  
  case object Shutdown
  
  ////////////////////////////////
  //
  // Internal API
  //
  
  /**
   * Persistent message: allocate a block of n OIDs in this Shard.
   */
  private case class Alloc(n:Int)
  
  /**
   * State of the Allocator, for snapshotting. 
   */
  private case class AllocState(availableThrough:Int)
}
