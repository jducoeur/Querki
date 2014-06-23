package querki.test

import scala.util.Try

import play.api.mvc.RequestHeader

import models.ThingId

import querki.ecology._
import querki.identity._
import querki.values.SpaceState

/**
 * Private trait, used to set up the "user database" for testing.
 */
trait UserTesting extends EcologyInterface {
  /**
   * Add the "users" defined in this Space.
   */
  def prepSpace(space:TestSpace)
}

class UserAccessStub(e:Ecology) extends QuerkiEcot(e) with UserAccess with UserTesting {
  // Stubs for functions we aren't using yet:
  def addSpaceMembership(identityId:OID, spaceId:OID):Boolean = ???
  def changePassword(requester:User, identity:Identity, newPassword:String):Try[User] = ???
  def changeDisplayName(requester:User, identity:Identity, newDisplay:String):Try[User] = ???
  def changeUserLevel(userId:OID, requester:User, level:UserLevel.UserLevel):Option[User] = ???
  def checkQuerkiLogin(login:String, passwordEntered:String):Option[User] = ???
  def createProvisional(info:SignupInfo):Try[User] = ???
  def get(request:RequestHeader):Option[User] = ???
  def getAllForAdmin(requester:User):Seq[User] = ???
  def getIdentity(rawHandle:String):Option[OID] = ???
  def getFullIdentity(id:IdentityId):Option[FullIdentity] = ???
  def getIdentity(thingId:ThingId):Option[(Identity, UserLevel.UserLevel)] = ???
  def getUserByHandleOrEmail(raw:String):Option[User] = ???
  def setTOSVersion(userId:OID, version:Int):Option[User] = ???
  
  // Implemented stubs
  def getIdentity(id:OID):Option[Identity] = identitiesById.get(id)
  
  var usersById:Map[OID, User] = Map.empty
  var identitiesById:Map[OID, Identity] = Map.empty
    
  def storePerson(u:User) = {
    usersById = usersById + (u.id -> u)
    u.identities.foreach(identity => identitiesById = identitiesById + (identity.id -> identity))
  }
  
  def prepSpace(space:TestSpace) = {
    storePerson(space.owner)
  }
}