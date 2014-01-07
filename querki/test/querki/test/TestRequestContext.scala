package querki.test

import models.{OID, Thing}

import querki.ecology._
import querki.identity.{Identity, IdentityKind, User}
import querki.identity.UserLevel._
import querki.system.TOSModule.noTOSUserVersion
import querki.values.{RequestContext, SpaceState}

case object BasicTestUser extends User {
  val id = models.system.OIDs.TestUserOID
  val name = "Test User"
  lazy val email = querki.email.EmailAddress("somebody@test.net")
  val identities = Seq(Identity(models.system.OIDs.TestIdentityOID, email, "", "Test User", name, IdentityKind.QuerkiLogin))
  val level = PaidUser
  val tosVersion = noTOSUserVersion  
}

case class SimpleTestRequestContext(o:OID, s:SpaceState, t:Thing, e:Ecology)(implicit requester:User = BasicTestUser) extends RequestContext(
    Some(requester),
    o,
    Some(s),
    Some(t),
    e)
{
  def withUpdatedState(newState:SpaceState):RequestContext = copy(s = newState)
}