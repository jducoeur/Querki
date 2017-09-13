package querki

import scala.concurrent.Future

import akka.actor._

import querki.ecology._
import querki.globals._

/**
 * @author jducoeur
 */
package object cluster {
  /**
   * The Querki-level identifier for a given Node. Mainly used in OID allocation.
   */
  type ShardId = Int
  
  trait QuerkiCluster extends EcologyInterface {
    /**
     * Pointer to the OIDAllocator instance for this node. Use this to obtain new OIDs.
     */
    def oidAllocator:ActorRef
    
    /**
     * The QuerkiNodeManager for this node. This is a node-local singleton that is,
     * among other things, responsible for listening to the overall state of the cluster
     * and managing this node's place within it.
     */
    def nodeManager:ActorRef
    
    def allocThingId()(implicit ecology:Ecology):Future[OID]
  }
}
