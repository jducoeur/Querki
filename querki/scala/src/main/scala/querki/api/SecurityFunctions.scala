package querki.api

import querki.data._

trait SecurityFunctions {
  /**
   * Fetch all of the Roles known to this Space, in their "usual" display order.
   */
  def getRoles():Seq[ThingInfo]
}
