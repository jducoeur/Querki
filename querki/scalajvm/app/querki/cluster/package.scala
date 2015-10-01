package querki

/**
 * @author jducoeur
 */
package object cluster {
  /**
   * The Querki-level identifier for a given Node. Mainly used in OID allocation.
   */
  type ShardId = Int
  
  private [cluster] final val singletonName = "querkiNodeSingleton"
  private [cluster] final val coordinatorName = "querkiNodeCoordinator"
  private [cluster] final val coordinatorPath = s"/user/$singletonName/$coordinatorName"
}
