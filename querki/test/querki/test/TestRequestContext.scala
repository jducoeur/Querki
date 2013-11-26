package querki.test

import models.{Thing}

import querki.identity.{Identity, IdentityKind, User}
import querki.identity.UserLevel._
import querki.system.TOSModule.noTOSUserVersion
import querki.values.{RequestContext, SpaceState}

case object BasicTestUser extends User {
  val id = models.system.OIDs.TestUserOID
  val name = "Test User"
  lazy val email = modules.email.EmailAddress("somebody@test.net")
  val identities = Seq(Identity(models.system.OIDs.TestIdentityOID, email, "", "Test User", name, IdentityKind.QuerkiLogin))
  val level = PaidUser
  val tosVersion = noTOSUserVersion  
}

case class SimpleTestRequestContext(s:SpaceState, t:Thing) extends RequestContext(
    Some(BasicTestUser),
    BasicTestUser.id,
    Some(s),
    Some(t))
{
  def withUpdatedState(newState:SpaceState):RequestContext = copy(s = newState)
}