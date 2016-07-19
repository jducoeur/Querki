package querki.identity

import querki.persistence._

object IdentityPersistence {
  
  /**
   * A reference to a user, suitable for serialization.
   */
  case class UserRef(@KryoTag(1) userId:UserId, @KryoTag(2) identityIdOpt:Option[IdentityId]) extends UseKryo

}
