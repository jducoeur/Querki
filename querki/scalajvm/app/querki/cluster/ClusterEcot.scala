package querki.cluster

import akka.actor._
import akka.contrib.pattern._

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
  
  private final val singletonName = "querkiNodeSingleton"
  private final val coordinatorName = "querkiNodeCoordinator"
  // TODO: that "querkiRoot" in there is sort of secret knowledge about the environment. That's a
  // bad smell. We should probably be getting this path's root from somewhere.
  private final val coordinatorPath = s"/user/querkiRoot/$singletonName/$coordinatorName"
  
  var _nodeCoordinator:Option[ActorRef] = None
  def nodeCoordinator = _nodeCoordinator.get
  
  var _nodeManager:Option[ActorRef] = None
  def nodeManager = _nodeManager.get
  // The OID allocator is a child of NodeManager, and routes through it.
  def oidAllocator = nodeManager
  
  override def createActors(createActorCb:CreateActorFunc) = {
    createActorCb(ClusterSingletonManager.props(
        QuerkiNodeCoordinator.actorProps(),
        coordinatorName,
        QuerkiNodeCoordinator.Stop,
        None
      ),
      singletonName)
      
    _nodeCoordinator = createActorCb(ClusterSingletonProxy.props(
          coordinatorPath,
          None), 
        "querkiNodeCoordinatorProxy")
        
    _nodeManager = createActorCb(QuerkiNodeManager.actorProps(ecology), "querkiNodeManager")
  }
}
