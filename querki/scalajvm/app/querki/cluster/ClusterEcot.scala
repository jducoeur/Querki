package querki.cluster

import akka.actor._
import akka.cluster.singleton._

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
  
  // TODO: until we have an Akka Persistence layer that we like, just don't even boot this up:
  // IMPORTANT: this has changed. See AdminEcot for how to deal with cluster singletons now:
//  override def createActors(createActorCb:CreateActorFunc) = {
//    createActorCb(ClusterSingletonManager.props(
//        QuerkiNodeCoordinator.actorProps(),
//        QuerkiNodeCoordinator.Stop,
//        ClusterSingletonManagerSettings(SystemManagement.actorSystem)
//      ),
//      singletonName)
//      
//    _nodeCoordinator = createActorCb(ClusterSingletonProxy.props(
//          coordinatorPath,
//          ClusterSingletonProxySettings(SystemManagement.actorSystem)), 
//        "querkiNodeCoordinatorProxy")
//        
//    _nodeManager = createActorCb(QuerkiNodeManager.actorProps(ecology), "querkiNodeManager")
//  }
}
