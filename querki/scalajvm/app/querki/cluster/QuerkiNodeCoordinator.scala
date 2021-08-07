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
class QuerkiNodeCoordinator(e: Ecology) extends PersistentActor with Requester with EcologyMember {

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
  private var shardAssignments = Map.empty[NodePath, ShardId]

  val defaultAddress = context.system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
  def nodePath(path: ActorPath): NodePath = NodePath(path.toStringWithAddress(defaultAddress))
  def nodePath(ref: ActorRef): NodePath = nodePath(ref.path)

  def doAssign(
    path: NodePath,
    shardId: ShardId
  ): ShardId = {
    QLog.spew(s"Assigning shard $shardId to node $path -- currently at $shardAssignments")
    // We increment the snapshotCounter here, so that it is updated correctly during
    // recovery:
    snapshotCounter += 1
    shardAssignments += (path -> shardId)
    shardId
  }

  def doUnassign(path: NodePath) = {
    QLog.spew(s"Removing shard assignment from node $path")
    shardAssignments -= path
    QLog.spew(s"Resulting shard assignments are $shardAssignments")
  }

  def assignShard(path: NodePath): ShardId = {
    val resultOpt =
      (0 until Int.MaxValue)
        .find { n => !fullShards.contains(n) && !shardAssignments.values.toSeq.contains(n) }
        .map { shardId: ShardId => doAssign(path, shardId) }

    resultOpt.getOrElse(throw new Exception("EMERGENCY: Querki is out of Shards! How is this possible?"))
  }

  def snap() = {
    QLog.spew(s"QuerkiNodeCoordinator saving snapshot")
    snapshotCounter = 0
    saveSnapshot(ShardSnapshot(fullShards, shardAssignments))
  }

  def makeAssignment(ref: ActorRef) = {
    val path = nodePath(ref)
    _assignmentsBeingChecked -= path
    QLog.spew(s"In makeAssignment($path)")
    // If we already have an entry, it is by definition stale since the Node
    // is asking for a new one:
    doUnassign(path)

    val assignment = assignShard(path)
    persist(ShardAssigned(path, assignment)) { msg =>
      sender ! ShardAssignment(assignment)
    }

    // Take occasional snapshots of the state:
    if (snapshotCounter >= snapshotInterval) {
      snap()
    }
  }

  def unassign(
    ref: ActorRef,
    reassign: Boolean
  ) = {
    val path = nodePath(ref)
    _assignmentsBeingChecked -= path
    QLog.spew(s"In unassign($path, $reassign)")
    shardAssignments.get(path).map { shardId =>
      persist(ShardUnassigned(path, shardId)) { msg =>
        doUnassign(path)
        if (reassign)
          makeAssignment(ref)
      }
    }
  }

  override protected def onRecoveryFailure(
    cause: Throwable,
    event: Option[Any]
  ): Unit = {
    cause.fillInStackTrace()
    QLog.error(s"QuerkiNodeCoordinator failed recovery, on event $event", cause)
    super.onRecoveryFailure(cause, event)
  }

  // This is used to avoid race conditions at startup: we initially ask the various nodes whether
  // they think they are currently assigned. But if they pro-actively *ask* us for an assignment,
  // we should invalidate that check:
  var _assignmentsBeingChecked = Set.empty[NodePath]

  val receiveRecover: Receive = {
    case OldShardAssigned(ref, assignment) => {
      // We pre-emptively reserve the Shard, and will sanity-check whether that's real on
      // RecoveryCompleted:
      QLog.spew(s"Recovered OldShardAssigned($ref, $assignment)")
      doAssign(nodePath(ref), assignment)
    }

    case ShardAssigned(path, assignment) => {
      // We pre-emptively reserve the Shard, and will sanity-check whether that's real on
      // RecoveryCompleted:
      QLog.spew(s"Recovered ShardAssigned($path, $assignment)")
      doAssign(path, assignment)
    }

    case OldShardUnassigned(ref, shardId) => {
      QLog.spew(s"Recovered OldShardUnassigned($ref, $shardId)")
      doUnassign(nodePath(ref))
    }

    case ShardUnassigned(path, shardId) => {
      QLog.spew(s"Recovered ShardUnassigned($path, $shardId)")
      doUnassign(path)
    }

    case ShardUnavailable(shardId) => {
      QLog.spew(s"Recovered ShardUnavailable($shardId)")
      fullShards += shardId
    }

    case SnapshotOffer(metadata, OldShardSnapshot(f, s)) => {
      QLog.spew(s"QuerkiNodeCoordinator recovering old snapshot; fullShards are $f; assignments are $s")
      fullShards = f
      shardAssignments = s.map { case (path, nodeId) => (nodePath(path), nodeId) }
    }

    case SnapshotOffer(metadata, ShardSnapshot(f, s)) => {
      QLog.spew(s"QuerkiNodeCoordinator recovering snapshot; fullShards are $f; assignments are $s")
      fullShards = f
      shardAssignments = s
    }

    case RecoveryCompleted => {
      QLog.spew(s"Finished recovering the QuerkiNodeCoordinator")
      // Okay -- now let's take a look at our assignments and see if they make sense:
      _assignmentsBeingChecked = shardAssignments.keySet
      shardAssignments.foreach { case (path, id) =>
        QLog.spew(s"CheckShardAssignment($id) for $path")
        context.system.actorSelection(ActorPath.fromString(path.s)).request(CheckShardAssignment(id)).onComplete {
          result =>
            // Do we still care about this request? If not, just ignore the result. This check will be false if
            // the node has already sent us a request for an assignment:
            if (_assignmentsBeingChecked.contains(path)) {
              result match {
                // Didn't get through to the Node:
                case Failure(ex) => {
                  // We don't have a ref, and have no way to get one, so we have to persist this with
                  // a snapshot instead:
                  QLog.spew(s"CheckShardAssignment($id) Failed")
                  doUnassign(path)
                  snap()
                }

                // This assignment is still active:
                case Success(ConfirmShardAssignment(ref)) => {
                  QLog.spew(s"CheckShardAssignment($id) Confirmed as $ref")
                  context.watch(ref)
                }

                // Found the node, but that's not the right assignment:
                case Success(RefuteShardAssignment(ref)) => {
                  QLog.spew(s"CheckShardAssignment($id) refuted by $ref; expected $path")
                  unassign(ref, false)
                }

                case other => QLog.error(
                    s"QuerkiNodeCoordinator.RecoveryCompleted, checking Shard Assignments, got unexpected response $other"
                  )
              }
            }
        }
      }
    }
  }

  val receiveCommand: Receive = LoggingReceive {
    case AssignShard(nodeRef) => {
      QLog.spew(s"Coordinator got AssignShard request from $nodeRef")
      context.watch(nodeRef)
      makeAssignment(nodeRef)
    }

    case ShardFull(shardId, nodeRef) => {
      QLog.spew(s"Coordinator got ShardFull($shardId, $nodeRef)")
      persist(ShardUnavailable(shardId)) { msg =>
        fullShards += shardId
      }
      // After we unassign, we should assign a new one immediately:
      unassign(nodeRef, true)
    }

    case Terminated(nodeRef) => {
      QLog.spew(s"Coordinator got Terminated($nodeRef)")
      unassign(nodeRef, false)
    }

    case SaveSnapshotSuccess(metadata)        => //QLog.spew(s"Successfully saved snapshot: $metadata")
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
  case class AssignShard(node: ActorRef)

  /**
   * Response from AssignShard -- this is the ID for the requesting Node to use.
   */
  case class ShardAssignment(shard: ShardId)

  /**
   * Sent by a NodeManager, to say that this Shard is exhausted. Response is a fresh ShardId.
   */
  case class ShardFull(
    shard: ShardId,
    node: ActorRef
  )

  /**
   * Sent by the Coordinator when it is recovering, to check its understanding of the world.
   */
  case class CheckShardAssignment(shard: ShardId)
  case class ConfirmShardAssignment(ref: ActorRef)
  case class RefuteShardAssignment(ref: ActorRef)

  ///////////////////////
  //
  // Internal API -- persisted events and state
  //

  def actorProps(e: Ecology) = Props(classOf[QuerkiNodeCoordinator], e)

  /**
   * This is what we are using instead of ActorPath. That's intentional: it is because there is no obvious
   * way to force ActorPath to think in terms of *absolute* path, and we only want to be using absolute paths,
   * that include addresses.
   */
  case class NodePath(@KryoTag(1) s: String) extends UseKryo

  /**
   * Sent by the ClusterSingletonManager to tell this copy to shut down for handoff.
   */
  case object Stop

  /**
   * Assignment of a ShardId to this Node.
   */
  case class OldShardAssigned(
    @KryoTag(1) nodePath: ActorRef,
    @KryoTag(2) shard: ShardId
  ) extends UseKryo

  case class ShardAssigned(
    @KryoTag(1) nodePath: NodePath,
    @KryoTag(2) shard: ShardId
  ) extends UseKryo

  /**
   * This Shard is now available.
   */
  case class OldShardUnassigned(
    @KryoTag(1) nodePath: ActorRef,
    @KryoTag(2) shard: ShardId
  ) extends UseKryo

  case class ShardUnassigned(
    @KryoTag(1) nodePath: NodePath,
    @KryoTag(2) shard: ShardId
  ) extends UseKryo

  /**
   * This Shard is now permanently unavailable.
   */
  case class ShardUnavailable(@KryoTag(1) shard: ShardId) extends UseKryo

  /**
   * The periodic snapshot of the assignment state, to make cluster startup faster.
   */
  case class OldShardSnapshot(
    @KryoTag(1) fullShards: Set[ShardId],
    @KryoTag(2) shardAssignments: Map[ActorPath, ShardId]
  ) extends UseKryo

  case class ShardSnapshot(
    @KryoTag(1) fullShards: Set[ShardId],
    @KryoTag(2) shardAssignments: Map[NodePath, ShardId]
  ) extends UseKryo
}
