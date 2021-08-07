package querki.test

import models.{OID, Thing}

import querki.ecology._
import querki.identity.{Identity, IdentityKind, User}
import querki.identity.UserLevel._
import querki.system.TOSModule.noTOSUserVersion
import querki.values.{RequestContext, SpaceState}

case object BasicTestUser extends User {
  import querki.identity.MOIDs._
  val id = TestUserOID
  val name = "Test User"
  lazy val email = querki.email.EmailAddress("somebody@test.net")
  val identities = Seq(Identity(TestIdentityOID, email, "", "Test User", name, IdentityKind.QuerkiLogin))
  val level = PaidUser
  val tosVersion = noTOSUserVersion
}

object SimpleTestRequestContext {

  def apply(
    o: OID,
    metadataOpt: Option[querki.api.RequestMetadata] = None
  )(implicit
    requester: User = BasicTestUser
  ) =
    RequestContext(Some(requester), o, metadataOpt = metadataOpt)
}
