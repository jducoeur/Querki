package querki

import akka.actor._

import querki.ecology._

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
  }
}
