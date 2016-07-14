package querki.cluster

import scala.collection.immutable.{HashMap, HashSet}
import scala.util.{Failure, Success}

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern._
import akka.persistence._

import scala.concurrent.duration._
import akka.util.Timeout

import org.querki.requester._

import querki.globals._
import querki.persistence._

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
  
  QLog.spew("Creating QuerkiNodeCoordinator")
  
  lazy val relookupTimeout = Timeout(Config.getDuration("querki.cluster.shardRelookupTimeout", 10 minutes))
  lazy val snapshotInterval = Config.getInt("querki.cluster.snapshotInterval", 100)
  
  /**
   * The Shards that are permanently out of action. Several are set aside for various
   * reasons; the rest get set through Persistence.
   */
  private var fullShards = Set[ShardId](0, 1, 2, 3)
  /**
   * Which Shard each Node currently owns. This map is intentionally by ActorPath, since
   * there should be only one assignment per Node. We don't want to stack up lots of
   * assignments to different ActorRefs that correspond to the same ActorPath.
   */
  private var shardAssignments = Map.empty[ActorPath, ShardId]
  
  def doAssign(ref:ActorRef, shardId:ShardId):ShardId = {
    QLog.spew(s"Assigning shard $shardId to node ${ref.path}")
    // We increment the snapshotCounter here, so that it is updated correctly during
    // recovery:
    snapshotCounter += 1
    shardAssignments += (ref.path -> shardId)
    shardId    
  }
  
  def doUnassign(path:ActorPath) = {
    QLog.spew(s"Removing shard assignment from node $path")
    shardAssignments -= path
  }
  
  def assignShard(ref:ActorRef):ShardId = {
    val resultOpt = 
      (0 until Int.MaxValue) 
      .find { n => !fullShards.contains(n) && !shardAssignments.values.toSeq.contains(n) } 
      .map { shardId:ShardId => doAssign(ref, shardId) }
    
    resultOpt.getOrElse(throw new Exception("EMERGENCY: Querki is out of Shards! How is this possible?"))
  }
  
  def makeAssignment(ref:ActorRef) = {
    // If we already have an entry, it is by definition stale since the Node
    // is asking for a new one:
    doUnassign(ref.path)
    
    val assignment = assignShard(ref)
    persist(ShardAssigned(ref, assignment)) { msg =>
      sender ! ShardAssignment(assignment)
    }
    
    // Take occasional snapshots of the state:
    if (snapshotCounter >= snapshotInterval) {
      QLog.spew(s"QuerkiNodeCoordinator saving snapshot")
      snapshotCounter = 0
      saveSnapshot(ShardSnapshot(fullShards, shardAssignments))
    }
  }
  
  def unassign(ref:ActorRef, reassign:Boolean) = {
    shardAssignments.get(ref.path) map { shardId =>
      persist(ShardUnassigned(ref, shardId)) { msg =>
        doUnassign(ref.path)
        if (reassign)
          makeAssignment(ref)
      }
    }
  }
  
  override protected def onRecoveryFailure(cause:Throwable, event:Option[Any]):Unit = {
    cause.fillInStackTrace()
    QLog.error(s"QuerkiNodeCoordinator failed recovery, on event $event", cause)
    super.onRecoveryFailure(cause, event)
  }
  
  val receiveRecover:Receive = {
    case ShardAssigned(ref, assignment) => {
      // We pre-emptively reserve the Shard, and will sanity-check whether that's real on
      // RecoveryCompleted:
      doAssign(ref, assignment)
    }
    
    case ShardUnassigned(ref, shardId) => {
      doUnassign(ref.path)
    }
    
    case ShardUnavailable(shardId) => {
      fullShards += shardId
    }
    
    case SnapshotOffer(metadata, ShardSnapshot(f, s)) => {
      QLog.spew(s"QuerkiNodeCoordinator recovering snapshot; fullShards are $f; assignments are $s")
      fullShards = f
      shardAssignments = s
    }
    
    case RecoveryCompleted => {
      QLog.spew(s"Finished recovering the QuerkiNodeCoordinator")
      // Okay -- now let's take a look at our assignments and see if they make sense:
      shardAssignments.foreach { case (path, id) =>
        context.system.actorSelection(path).request(CheckShardAssignment(id)) onComplete {
          // Didn't get through to the Node:
          case Failure(ex) => doUnassign(path)
          
          // This assignment is still active:
          case Success(ConfirmShardAssignment(ref)) => context.watch(ref)
          
          // Found the node, but that's not the right assignment:
          case Success(RefuteShardAssignment) => {
            // This one's subtle because of potential races -- only unassign if that's *still*
            // the assignment. If not, it probably restarted and got a new assignment while this
            // was out there:
            if (shardAssignments.get(path) == Some(id))
              doUnassign(path)
          }
          
          case other => QLog.error(s"QuerkiNodeCoordinator.RecoveryCompleted, checking Shard Assignments, got unexpected response $other")
        }
      }
    }
  }
  
  val receiveCommand:Receive = LoggingReceive {
    case AssignShard(nodeRef) => {
      QLog.spew("Coordinator got AssignShard request")
      context.watch(nodeRef)
      makeAssignment(nodeRef)
    }

    case ShardFull(shardId, nodeRef) => {
      persist(ShardUnavailable(shardId)) { msg =>
        fullShards += shardId
      }
      // After we unassign, we should assign a new one immediately:
      unassign(nodeRef, true)
    }
    
    case Terminated(nodeRef) => {
      unassign(nodeRef, false)
    }
    
    case SaveSnapshotSuccess(metadata) => //QLog.spew(s"Successfully saved snapshot: $metadata")
    case SaveSnapshotFailure(metadata, cause) => QLog.error(s"Failed to save snapshot: $metadata", cause)
    
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
  case class AssignShard(node:ActorRef)
  /**
   * Response from AssignShard -- this is the ID for the requesting Node to use.
   */
  case class ShardAssignment(shard:ShardId)
  
  /**
   * Sent by a NodeManager, to say that this Shard is exhausted. Response is a fresh ShardId.
   */
  case class ShardFull(shard:ShardId, node:ActorRef)
  
  /**
   * Sent by the Coordinator when it is recovering, to check its understanding of the world.
   */
  case class CheckShardAssignment(shard:ShardId)
  case class ConfirmShardAssignment(ref:ActorRef)
  case object RefuteShardAssignment
  
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
  case class ShardAssigned(@KryoTag(1) nodePath:ActorRef, @KryoTag(2) shard:ShardId) extends UseKryo
  
  /**
   * This Shard is now available.
   */
  case class ShardUnassigned(@KryoTag(1) nodePath:ActorRef, @KryoTag(2) shard:ShardId) extends UseKryo
  
  /**
   * This Shard is now permanently unavailable.
   */
  case class ShardUnavailable(@KryoTag(1) shard:ShardId) extends UseKryo
  
  /**
   * The periodic snapshot of the assignment state, to make cluster startup faster.
   */
  case class ShardSnapshot(@KryoTag(1) fullShards:Set[ShardId], @KryoTag(2) shardAssignments:Map[ActorPath, ShardId]) extends UseKryo
}
