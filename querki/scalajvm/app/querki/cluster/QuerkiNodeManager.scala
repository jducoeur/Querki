package querki.cluster

import scala.concurrent.duration._
import scala.util.{Failure, Success}

import akka.actor._
import akka.cluster._
import ClusterEvent._
import akka.pattern.AskTimeoutException

import org.querki.requester._

import querki.ecology._
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
class QuerkiNodeManager(implicit val ecology:Ecology) extends Actor with Stash with Requester 
  with EcologyMember with querki.system.AsyncInittingActor[QuerkiNodeManager]
{
  import QuerkiNodeCoordinator._
  import QuerkiNodeManager._
  
  lazy val ClusterPrivate = interface[ClusterPrivate]
  
  lazy val clusterSize = Config.getInt("querki.cluster.size")
  // Quorum is at least half of the cluster size:
  lazy val quorum = (clusterSize / 2) + 1
  
  var _shardId:Option[ShardId] = None
  def shardId = _shardId.get
  
  var _allocator:Option[ActorRef] = None
  def allocator = _allocator.get
  
  def requestShardId(msg:Any):Unit = {
    QLog.spew(s"QuerkiNodeManager sending $msg to Coordinator")
    val reqM = ClusterPrivate.nodeCoordinator.request(msg) onComplete
    {
      case Success(ShardAssignment(id)) => {
        _shardId = Some(id)
        _allocator = Some(context.actorOf(OIDAllocator.actorProps(ecology, shardId), "OIDAllocator"))
        initted()
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
        requestShardId(msg)
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
    requestShardId(AssignShard(self))
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
    // This looks a bit weird, but is (I think) necessary due to the equality operation of Member.
    // Since members is a *Set*, and the state doesn't get detected as an equality difference, then
    // simply using "+" won't be detected as needing to change anything. So we need to remove the old
    // version of this Member, and then add the new one.
    updateState("Adding/Updating", mem, {cs => cs.copy(members = (cs.members - mem) + mem)})
  }
  def ownStatus:Option[MemberStatus] = {
    for {
      state <- _clusterState
      selfMem <- state.members.find(_.uniqueAddress == selfUniqueAddress)
    }
      yield selfMem.status
  }
    
  /**
   * How long to wait after something goes unreachable, before we invoke Split Brain Resolution.
   */
  lazy val unreachableTimeout = Config.getDuration("querki.cluster.unreachable.timeout", 20 seconds)
  var unreachableTimers:Map[Member, Cancellable] = Map.empty
  /**
   * When a Member becomes unreachable, we give it a little while to see if it comes back. If it
   * doesn't, we invoke Split-Brain Resolution.
   */
  def startUnreachableClock(mem:Member) = {
    val cancel = context.system.scheduler.scheduleOnce(unreachableTimeout, self, HandleUnreachable(mem))
    unreachableTimers += (mem -> cancel)
  }
  def stopUnreachableClock(mem:Member) = {
    unreachableTimers.get(mem) map { cancel =>
      cancel.cancel()
      unreachableTimers -= mem
    }
  }
  
  /**
   * SPLIT-BRAIN RESOLUTION
   * 
   * This is a very simplistic SBR, based on the keep-majority principle. When at least one Member has
   * become Unreachable for a while, we check which side of the fence we seem to be on. If we can still
   * see a quorum of Members, then we down that unreachable Member. If we *can't* see a quorum, then we
   * presume that we are on the wrong side of a partition, and we shoot ourselves in the head.
   */
  def resolveSplit(mem:Member) = {
    for {
      state <- _clusterState
      app <- PlayEcology.maybeApplication
    }
    {
      val nUnreachables = state.unreachable.size
      val nReachables = state.members.size - nUnreachables
      if (nReachables < quorum) {
        // We're on the wrong side -- kill ourselves!
        QLog.spew(s"SBR: quorum is $quorum, and we can only see $nReachables, so I'm shooting myself in the head!")
        app.stop()
      } else {
        // We can still see quorum, so down the unreachable Member
        QLog.spew(s"SBR: quorum is $quorum, and I can still see $nReachables, so downing $mem")
        Cluster(context.system).down(mem.address)
      }
    }
  }
  
  def receive = handleRequestResponse orElse {
    case OIDAllocator.NextOID => {
      _allocator match {
        case Some(alloc) => alloc.forward(OIDAllocator.NextOID)
        case None => stash()
      }
    }
    
    case msg @ ShardFull(id, _) => {
      // If the ID doesn't match the current one, this message is probably out of date:
      if (id == shardId) {
        // The OIDAllocator is reporting that it is full, so we need to obtain a new Shard ID.
        // This will result in a new ShardAssignment. In the meantime, go back to stashing until
        // we have that:
        allocator ! OIDAllocator.Shutdown
        _allocator = None
        _shardId = None
        // Note that we are actually sending a ShardFull here; once that is complete, it should
        // kick off the reassignment and hand us a new ShardId. Also, note that we substitute our
        // own ActorRef into here, since this Manager is what the Coordinator really thinks in
        // terms of:
        requestShardId(msg.copy(node = self))
      }
    }
    
    // The Coordinator apparently restarted, and is sounding us out about the actual state
    // of the world:
    case CheckShardAssignment(id) => {
      if (_shardId.map(_ == id).getOrElse(false)) {
        sender ! ConfirmShardAssignment(self)
      } else {
        sender ! RefuteShardAssignment
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
      startUnreachableClock(member)
    }
    case ReachableMember(member) => {
      updateState("Returning to reachable", member, {cs => cs.copy(unreachable = cs.unreachable - member)})
      stopUnreachableClock(member)
    }
    
    case HandleUnreachable(member) => {
      QLog.spew(s"Member $member is persistently unreachable, so invoking Split-Brain!")
      resolveSplit(member)
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
  
  private case class HandleUnreachable(mem:Member)
}
