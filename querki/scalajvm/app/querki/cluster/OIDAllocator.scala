package querki.cluster

import akka.actor._
import akka.pattern._
import akka.persistence._

import org.querki.requester._

import models.OID

import querki.globals._
import querki.persistence._

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
class OIDAllocator(e:Ecology, shardId:ShardId) extends PersistentActor with Requester with EcologyMember {
  import OIDAllocator._
  
  implicit val ecology = e
  
  override def persistenceId = s"alloc$shardId"
  
  lazy val allocBlockSize = Config.getInt("querki.cluster.allocBlockSize", 10)
  lazy val allocSnapshotInterval = Config.getInt("querki.cluster.allocSnapshotInterval", 100)
  lazy val shardFullBufferSize = Config.getInt("querki.cluster.shardFullBufferSize", 1000)
  // When we hit the shardFullMark, raise the alarm that we need to shut down this shard:
  lazy val shardFullMark = Int.MaxValue - shardFullBufferSize
  
  var current:Int = 0
  var availableThrough:Int = 0
  var snapshotCount:Int = 0
  
  def updateAvailable(blockSize:Int) = {
    // Make sure we avoid overflows:
    availableThrough = Math.min(availableThrough + blockSize, Int.MaxValue)
  }
  
  def countToSnapshot() = {
    snapshotCount += 1
    if (snapshotCount >= allocSnapshotInterval) {
      snapshotCount = 0
      saveSnapshot(AllocState(availableThrough))
    }
  }
  
  val receiveRecover:Receive = {
    case Alloc(blockSize) => {
      updateAvailable(blockSize)
      current = availableThrough
      countToSnapshot()
    }
    
    case SnapshotOffer(metadata, AllocState(a)) => {
      availableThrough = a
      current = availableThrough
    }
    
    case RecoveryCompleted => {
      // We don't currently need to do anything at the end of recovery
    }
    
    case other => QLog.error(s"OIDAllocator got unexpected message $other")
  }
  
  def sendFull(n:Int):Unit = {
    if (n > 3)
      // Give up on shutting down cleanly, and just die...
      throw new Exception(s"OIDAllocator $shardId unable to send ShardFull! Dying...")
    
    context.parent.request(QuerkiNodeCoordinator.ShardFull(shardId, self)) map {
      case Shutdown => context.stop(self)
      case _ => sendFull(n + 1)
    }    
  }
  
  val receiveCommand:Receive = {
    case NextOID => {
      if (current == Int.MaxValue) {
        // Emergency! At this point we just need to fall over.
        context.parent ! QuerkiNodeCoordinator.ShardFull(shardId, self)
        throw new Exception(s"Overfull OIDAllocator $shardId")
      }

      def giveOID() = {        
        val oid = OID(shardId, current)
        sender ! NewOID(oid)        
        current += 1
        
        if (current == shardFullMark) {
          // Time to begin shutting down this shard...
          sendFull(0)
        }
      }
      
      if (current >= availableThrough) {
        // We've used up the current allocation, so allocate more:
        persist(Alloc(allocBlockSize)) { msg =>
          updateAvailable(allocBlockSize)
          giveOID()
          countToSnapshot()
        }
      } else {
        // We're still in the current block:
        giveOID()
      }
    }
    
    case SaveSnapshotSuccess(metadata) => //QLog.spew(s"Successfully saved snapshot: $metadata")
    case SaveSnapshotFailure(metadata, cause) => QLog.error(s"Failed to save snapshot: $metadata", cause)
  }
}

object OIDAllocator {
  def actorProps(e:Ecology, shardId:ShardId) = Props(classOf[OIDAllocator], e, shardId)
  
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
  case class Alloc(@KryoTag(1) val n:Int) extends UseKryo
  
  /**
   * State of the Allocator, for snapshotting. 
   */
  case class AllocState(@KryoTag(1) availableThrough:Int) extends UseKryo
}
