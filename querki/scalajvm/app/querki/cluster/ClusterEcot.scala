package querki.cluster

import akka.actor._
import akka.cluster.singleton._
import akka.pattern._

import querki.ecology._
import querki.globals._

/**
 * Internal API within the Querki Cluster subsystem.
 */
private [cluster] trait ClusterPrivate extends EcologyInterface {
  /**
   * Pointer to the QuerkiNodeCoordinator. Note that this is actually an indirection,
   * but you can treat it as an ordinary ActorRef -- just keep in mind that it will
   * often be remote.
   */
  def nodeCoordinator:ActorRef
}

/**
 * @author jducoeur
 */
class ClusterEcot(e:Ecology) extends QuerkiEcot(e) with ClusterPrivate with QuerkiCluster {
  
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  // For the moment, this needs to be turned on for us to use the new object-creation mechanism,
  // which is the main purpose of the QuerkiNodeCoordinator/Manager system:
  lazy val newObjCreate = Config.getBoolean("querki.cluster.newObjCreate", false)
  
  private final val singletonName = "querkiNodeSingleton"
  private final val coordinatorName = "querkiNodeCoordinator"
  // TODO: that "querkiRoot" in there is sort of secret knowledge about the environment. That's a
  // bad smell. We should probably be getting this path's root from somewhere.
  private final val coordinatorPath = s"/user/querkiRoot/$singletonName"
  
  var _nodeCoordinator:Option[ActorRef] = None
  def nodeCoordinator = _nodeCoordinator.get
  
  var _nodeManager:Option[ActorRef] = None
  def nodeManager = _nodeManager.get
  // The OID allocator is a child of NodeManager, and routes through it.
  def oidAllocator = nodeManager
  
  override def persistentMessages = persist(58,
    (classOf[OIDAllocator.Alloc] -> 100),
    (classOf[OIDAllocator.AllocState] -> 101),
    
    (classOf[QuerkiNodeCoordinator.OldShardAssigned] -> 201),
    (classOf[QuerkiNodeCoordinator.OldShardUnassigned] -> 202),
    (classOf[QuerkiNodeCoordinator.ShardUnavailable] -> 203),
    (classOf[QuerkiNodeCoordinator.OldShardSnapshot] -> 204),
    
    (classOf[QuerkiNodeCoordinator.NodePath] -> 205),
    (classOf[QuerkiNodeCoordinator.ShardAssigned] -> 206),
    (classOf[QuerkiNodeCoordinator.ShardUnassigned] -> 207),
    (classOf[QuerkiNodeCoordinator.ShardSnapshot] -> 208)
  )
  
  override def createActors(createActorCb:CreateActorFunc) = {
    if (newObjCreate) {
      val (mgr, proxy) = SystemManagement.createClusterSingleton(
        createActorCb, 
        QuerkiNodeCoordinator.actorProps(ecology), 
        singletonName, 
        "querkiNodeCoordinatorProxy", 
        QuerkiNodeCoordinator.Stop)
        
      _nodeCoordinator = proxy
      _nodeManager = createActorCb(QuerkiNodeManager.actorProps(ecology), "querkiNodeManager")
      
      regAsyncInit[QuerkiNodeManager]
    }
  }
  
  def allocThingId()(implicit ecology:Ecology):Future[OID] = {
    import OIDAllocator._
    import querki.util.ActorHelpers._
    // Since we are not directly inside an Actor, we use ask instead of request:
    oidAllocator.ask(NextOID)(timeout).map { case NewOID(thingId) => thingId }
  }
}
