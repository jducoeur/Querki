package querki.identity

import querki.globals._
import querki.persistence._

object IdentityPersistence {

  /**
   * A reference to a user, suitable for serialization.
   */
  case class UserRef(
    @KryoTag(1) userId: UserId,
    @KryoTag(2) identityIdOpt: Option[IdentityId]
  ) extends UseKryo

  val SystemUserRef = UserRef(MOIDs.SystemUserOID, None)
}

trait IdentityPersistence extends EcologyMember {
  import IdentityPersistence._

  private lazy val Person = interface[querki.identity.Person]
  implicit def user2Ref(user: User)(implicit state: SpaceState): UserRef = Person.user2Ref(user)
}
