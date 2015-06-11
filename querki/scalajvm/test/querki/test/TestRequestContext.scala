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

case class SimpleTestRequestContext(o:OID, qs:Map[String,Seq[String]] = Map.empty)(implicit requester:User = BasicTestUser) extends RequestContext(
    Some(requester),
    o)
{
  def queryParam(paramName:String):Seq[String] = qs(paramName)
}
