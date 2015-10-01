package querki.cluster

import akka.contrib.pattern.ClusterSingletonManager

import querki.ecology._
import querki.globals._

/**
 * @author jducoeur
 */
class ClusterEcot(e:Ecology) extends QuerkiEcot(e) {
  override def createActors(createActorCb:CreateActorFunc) = {
    createActorCb(ClusterSingletonManager.props(
        QuerkiNodeCoordinator.actorProps(),
        coordinatorName,
        QuerkiNodeCoordinator.Stop,
        None
      ),
      singletonName)
  }
}
