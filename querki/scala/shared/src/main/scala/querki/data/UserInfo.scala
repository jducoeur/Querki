package querki.data

import upickle.default.{macroRW, ReadWriter => RW}

case class IdentityInfo(
  oid: String,
  name: String,
  handle: String
)

object IdentityInfo {
  implicit val rw: RW[IdentityInfo] = macroRW
}

case class UserInfo(
  oid: String,
  identities: Seq[IdentityInfo],
  skillLevel: TID,
  actualUser: Boolean
) {
  def mainIdentity = identities.head
}

object UserInfo {
  implicit val rw: RW[UserInfo] = macroRW
}
