package querki.cluster

import scala.util.{Failure, Success}

import akka.actor._
import akka.cluster._
import ClusterEvent._
import akka.pattern.AskTimeoutException

import org.querki.requester._

import querki.globals._

/**
 * This Actor is responsible for Querki's view of the Cluster.
 * 
 * It is specifically responsible for working with the QuerkiNodeCoordinator (a central
 * Cluster Singleton) to get a Shard ID for this node. That ID will then be used for
 * OID creation on this node, via the OIDAllocator.
 * 
 * It also listens to the state of the Cluster and its members.
 * 
 * IMPORTANT: this is currently responsible for implementing our simple split-brain
 * resolution!
 * 
 * @author jducoeur
 */
class QuerkiNodeManager(implicit val ecology:Ecology) extends Actor with Stash with Requester with EcologyMember {
  
  import QuerkiNodeCoordinator._
  
  lazy val ClusterPrivate = interface[ClusterPrivate]
  
  lazy val clusterSize = Config.getInt("querki.cluster.size")
  // Quorum is at least half of the cluster size:
  // TBD: is this over-complicated? Can I rely on Integer math to always produce the
  // floor, and then add 1?
  lazy val quorum = {
    val isEven = (clusterSize % 2) == 0
    val halfSize = clusterSize.toDouble / 2.toDouble
    if (isEven)
      // It's an even number, so quorum is *more* than half:
      halfSize.toInt + 1
    else
      // Not an even number, so just round up
      halfSize.ceil.toInt
  }
  
  var _shardId:Option[ShardId] = None
  def shardId = _shardId.get
  
  var _allocator:Option[ActorRef] = None
  def allocator = _allocator.get
  
  def requestShardId():Unit = {
    val reqM = ClusterPrivate.nodeCoordinator.request(AssignShard()) onComplete
    {
      case Success(ShardAssignment(id)) => {
        _shardId = Some(id)
        _allocator = Some(context.actorOf(OIDAllocator.actorProps(ecology, shardId), "OIDAllocator"))
        unstashAll()        
      }

      case Failure(ex:AskTimeoutException) => {
        // For the time being, we're being very simplistic and retrying endlessly. This is mainly
        // because I don't have a better solution yet -- if this has *really* broken down, it's a fatal
        // system panic. Hopefully the Coordinator will come back.
        //
        // Note that this timeout is downright normal when we're starting up the seed node. The
        // QuerkiNodeManager tends to ask for its shard before the Coordinator singleton has been
        // fully created, and the ClusterSingleton mechanism appears to just drop the request on the floor.
        QLog.warn(s"QuerkiNodeManager: AssignShard timed out; trying again")
        requestShardId()
      }
      
      case other => {
        QLog.error(s"QuerkiNodeManager got unexpected response $other from AssignShard!")
      }
    }
  }
  
  override def preStart() = {
    // While we're inside of a context, make sure we have our own address:
    selfUniqueAddress
    Cluster(context.system).subscribe(self, classOf[ReachabilityEvent], classOf[MemberEvent])
    // TODO: turn this back on
//    requestShardId()
  }

  lazy val selfUniqueAddress = Cluster(context.system).selfUniqueAddress
  var _clusterState:Option[CurrentClusterState] = None
  def setState(s:CurrentClusterState) = {
    _clusterState = Some(s)
    ownStatus.foreach(status => QLog.spew(s"I am currently $status"))    
  }
  def updateState(op:String, mem:Member, f:CurrentClusterState => CurrentClusterState) = {
    QLog.spew(s"$op member $mem")
    val cs = _clusterState.get
    setState(f(cs))
  }
  def removeMember(mem:Member) = {
    updateState("Removing", mem, {cs => cs.copy(members = cs.members - mem)})
  }
  def updateMember(mem:Member) = {
    updateState("Adding/Updating", mem, {cs => cs.copy(members = cs.members + mem)})
  }
  def ownStatus:Option[MemberStatus] = {
    for {
      state <- _clusterState
      selfMem <- state.members.find(_.uniqueAddress == selfUniqueAddress)
    }
      yield selfMem.status
  }
  
  def receive = handleRequestResponse orElse {
    case OIDAllocator.NextOID => {
      _allocator match {
        case Some(alloc) => alloc.forward(OIDAllocator.NextOID)
        case None => stash()
      }
    }
    
    case msg @ ShardFull(id) => {
      // If the ID doesn't match the current one, this message is probably out of date:
      if (id == shardId) {
        // The OIDAllocator is reporting that it is full, so we need to obtain a new Shard ID.
        // This will result in a new ShardAssignment. In the meantime, go back to stashing until
        // we have that:
        ClusterPrivate.nodeCoordinator ! msg
        allocator ! OIDAllocator.Shutdown
        _allocator = None
        _shardId = None
        requestShardId()
      }
    }
    
    /****************************************
     * 
     * Cluster Events
     * 
     */
    case s @ CurrentClusterState(mem, unreachable, seenBy, leader, roleLeaderMap) => {
      QLog.spew(s"CurrentClusterState = $s")
      setState(s)
    }
    
    case MemberExited(member) => removeMember(member)
    case MemberJoined(member) => updateMember(member)
    case MemberLeft(member) => removeMember(member)
    case MemberRemoved(member, prev) => removeMember(member)
    case MemberUp(member) => updateMember(member)
    case MemberWeaklyUp(member) => updateMember(member)
    
    case UnreachableMember(member) => {
      updateState("Marking unreachable", member, {cs => cs.copy(unreachable = cs.unreachable + member)})
    }
    case ReachableMember(member) => {
      updateState("Returning to reachable", member, {cs => cs.copy(unreachable = cs.unreachable - member)})
    }
    
    case QuerkiNodeManager.RequestState => sender ! _clusterState
  }
}

object QuerkiNodeManager {
  def actorProps(ecology:Ecology) = Props(classOf[QuerkiNodeManager], ecology)
  
  /**
   * Returns an Option[CurrentClusterState].
   */
  case object RequestState
}
