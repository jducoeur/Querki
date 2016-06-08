package querki.cluster

import scala.util.{Failure, Success}

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern._
import akka.persistence._

import scala.concurrent.duration._
import akka.util.Timeout

import org.querki.requester._

import querki.globals._

/**
 * The central gatekeeper for Querki's "shards".
 * 
 * Each node in Querki is a single "shard", in the sense that it owns a chunk of the global
 * OID namespace. Those shards are represented by QuerkiNodeManager. This is the central
 * coordinator for all of them, responsible for assigning shards to nodes. It is a Cluster
 * Singleton -- there should be exactly one of these running at any given time. (Modulo
 * occasional downtime when the Coordinator has to move.) It is *extremely* low-traffic,
 * since each node should contact it only at startup time.
 * 
 * This is a PersistentActor -- the shard assignments are maintained persistently, even
 * across restarts. It is intentionally conservative about deciding that a node is dead.
 * The operating theory is that we can afford to let the shard namespace be slightly sparse:
 * it is much, much better to let a shard ID go unused for a little while than wind up with
 * two nodes thinking they have the same shard.
 * 
 * TODO: this currently uses its own timeouts to decide when a node is dead. That may be
 * wrong -- I suspect we should instead be using the Cluster system itself to make that decision,
 * and we should be listening to its Down notifications.
 * 
 * @author jducoeur
 */
class QuerkiNodeCoordinator(e:Ecology) extends PersistentActor with Requester with EcologyMember {
  
  import QuerkiNodeCoordinator._
  
  implicit val ecology = e
  
  override def persistenceId = "nodeCoordinator"
  
  private var snapshotCounter = 0
  
  lazy val relookupTimeout = Timeout(Config.getDuration("querki.cluster.shardRelookupTimeout", 10 minutes))
  lazy val snapshotInterval = Config.getInt("querki.cluster.snapshotInterval", 100)
  
  /**
   * The Shards that are permanently out of action. Several are set aside for various
   * reasons; the rest get set through Persistence.
   */
  private var fullShards = Set[ShardId](0, 1, 2, 3)
  /**
   * Which Shard each Node currently owns.
   */
  private var shardAssignments = Map.empty[ActorPath, ShardId]
  
  def doAssign(path:ActorPath, shardId:ShardId):ShardId = {
    shardAssignments += (path -> shardId)
    shardId    
  }
  
  def doUnassign(path:ActorPath, shardId:ShardId) = {
    shardAssignments -= path
  }
  
  def assignShard():ShardId = {
    val resultOpt = 
      (0 until Int.MaxValue) 
      .find { n => !fullShards.contains(n) && !shardAssignments.values.toSeq.contains(n) } 
      .map { shardId:ShardId => doAssign(sender.path, shardId) }
    
    resultOpt.getOrElse(throw new Exception("EMERGENCY: Querki is out of Shards! How is this possible?"))
  }
  
  def makeAssignment() = {
    val assignment = assignShard()
    persist(ShardAssigned(sender.path, assignment)) { msg =>
      sender ! ShardAssignment(assignment)
    }
    
    // Take occasional snapshots of the state:
    snapshotCounter += 1
    if (snapshotCounter >= snapshotInterval) {
      snapshotCounter = 0
      saveSnapshot(ShardSnapshot(fullShards, shardAssignments))
    }
  }
  
  def unassign(path:ActorPath, reassign:Boolean) = {
    shardAssignments.get(path) map { shardId =>
      persist(ShardUnassigned(path, shardId)) { msg =>
        doUnassign(path, shardId)
        if (reassign)
          makeAssignment()
      }
    }
  }
  
  val receiveRecover:Receive = {
    case ShardAssigned(path, assignment) => {
      // We pre-emptively reserve the Shard...
      doAssign(path, assignment)
      
      // ... then figure out whether it's still real:
      val sel = context.actorSelection(path)
      // relookupTimeout doesn't have to be aggressive, since we're willing to let the
      // Shard namespace be a little sparse:
      sel.request(Identify(assignment)) onComplete {
        case Failure(ex) => {
          // We can't seem to find it, so give up on the assignment
          doUnassign(path, assignment)
        }

        case Success(ActorIdentity(_, refOpt)) => {
          refOpt match {
            case Some(ref) => {
              // We're set, so start watching that Shard
              context.watch(ref)
            }
            case None => {
              // Positive failure to find that path
              doUnassign(path, assignment)
            }
          }
        }
        
        case other => QLog.error(s"QuerkiNodeCoordinator got unexpected response from Identify: $other")
      }
    }
    
    case ShardUnassigned(path, shardId) => {
      doUnassign(path, shardId)
    }
    
    case ShardUnavailable(shardId) => {
      fullShards += shardId
    }
    
    case SnapshotOffer(metadata, ShardSnapshot(f, s)) => {
      fullShards = f
      shardAssignments = s
    }
  }
  
  val receiveCommand:Receive = LoggingReceive {
    case AssignShard() => {
      context.watch(sender)
      makeAssignment()
    }

    case ShardFull(shardId) => {
      persist(ShardUnavailable(shardId)) { msg =>
        fullShards += shardId
      }
      // After we unassign, we should assign a new one immediately:
      unassign(sender.path, true)
    }
    
    case Terminated(nodeRef) => {
      unassign(nodeRef.path, false)
    }
    
    case Stop => {
      context.stop(self)
    }
  }
}

object QuerkiNodeCoordinator {
  ///////////////////////
  //
  // Public API
  //
  
  /**
   * Sent from a NodeManager, asking the QuerkiNodeCoordinator to give it an available shard ID.
   */
  case class AssignShard()
  /**
   * Response from AssignShard -- this is the ID for the requesting Node to use.
   */
  case class ShardAssignment(shard:ShardId)
  
  /**
   * Sent by a NodeManager, to say that this Shard is exhausted. Response is a fresh ShardId.
   */
  case class ShardFull(shard:ShardId)
  
  ///////////////////////
  //
  // Internal API -- persisted events and state
  //
  
  def actorProps(e:Ecology) = Props(classOf[QuerkiNodeCoordinator], e)
  
  /**
   * Sent by the ClusterSingletonManager to tell this copy to shut down for handoff.
   */
  case object Stop
  
  /**
   * Assignment of a ShardId to this Node.
   */
  case class ShardAssigned(nodePath:ActorPath, shard:ShardId)
  
  /**
   * This Shard is now available.
   */
  case class ShardUnassigned(nodePath:ActorPath, shard:ShardId)
  
  /**
   * This Shard is now permanently unavailable.
   */
  case class ShardUnavailable(shard:ShardId)
  
  /**
   * The periodic snapshot of the assignment state, to make cluster startup faster.
   */
  case class ShardSnapshot(fullShards:Set[ShardId], shardAssignments:Map[ActorPath, ShardId])
}
