package querki.security

import querki.data._

case class RoleInfo(
  map: Map[TID, ThingInfo],
  roles: Seq[ThingInfo]
) {
  def default = roles.head
  def isEmpty = roles.find(_.oid.underlying.length > 0).isEmpty
}
