package querki.cluster

import akka.actor._

import querki.globals._

/**
 * This Actor is responsible for Querki's view of the Cluster.
 * 
 * It is specifically responsible for working with the QuerkiNodeCoordinator (a central
 * Cluster Singleton) to get a Shard ID for this node. That ID will then be used for
 * OID creation on this node, via the OIDAllocator.
 * 
 * @author jducoeur
 */
class QuerkiNodeManager(val ecology:Ecology) extends Actor with Stash with EcologyMember {
  
  import QuerkiNodeCoordinator._
  
  lazy val ClusterPrivate = interface[ClusterPrivate]
  
  var _shardId:Option[ShardId] = None
  def shardId = _shardId.get
  
  var _allocator:Option[ActorRef] = None
  def allocator = _allocator.get
  
  override def preStart() = {
    ClusterPrivate.nodeCoordinator ! AssignShard()
  }
  
  def receive = {
    case ShardAssignment(id) => {
      _shardId = Some(id)
      _allocator = Some(context.actorOf(OIDAllocator.actorProps(shardId), "OIDAllocator"))
      unstashAll()
    }
    
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
      }
    }
  }
}

object QuerkiNodeManager {
  def actorProps(ecology:Ecology) = Props(classOf[QuerkiNodeManager], ecology)
}
