package querki.security

import autowire._

import querki.data._
import querki.globals.execContext
import querki.test.mid._

import SecurityFunctions._

import AllFuncs._

/**
 * Not included in AllFuncs, since the functions here are relatively specialized and I don't want to pollute that
 * namespace too much. Import or extend this explicitly if you want these.
 */
trait SecurityMidFuncs {
  
  ///////////////////////////////////
  //
  // Wrappers around the SecurityFunctions API.
  //
  
  def getSecurityInfo(): TestOp[SpaceSecurityInfo] =
    TestOp.client { _[SecurityFunctions].getSecurityInfo().call() }

  def getRoles(): TestOp[(Seq[ThingInfo], Seq[ThingInfo])] =
    TestOp.client { _[SecurityFunctions].getRoles().call() }
  
  def getMembers(): TestOp[(Seq[PersonInfo], Seq[PersonInfo])] =
    TestOp.client { _[SecurityFunctions].getMembers().call() }
  
  def invite(emails:Seq[String], collabs:Seq[TID]): TestOp[InviteResponse] =
    TestOp.client { _[SecurityFunctions].invite(emails, collabs).call() }
  
  def archiveThisSpace(): TestOp[Boolean] =
    TestOp.client { _[SecurityFunctions].archiveThisSpace().call() }
  
  def permsFor(thing:TID): TestOp[ThingPermissions] =
    TestOp.client { _[SecurityFunctions].permsFor(thing).call() }
  
  def getOnePerm(id:TID): TestOp[PermInfo] =
    TestOp.client { _[SecurityFunctions].getOnePerm(id).call() }
  
  def getAllPerms(): TestOp[Seq[PermInfo]] =
    TestOp.client { _[SecurityFunctions].getAllPerms().call() }
  
  def getSharedLinksForRole(roleId: TOID): TestOp[Seq[SharedLinkInfo]] =
    TestOp.client { _[SecurityFunctions].getSharedLinksForRole(roleId).call() }
  
  def getOneSharedLink(linkId: TOID): TestOp[SharedLinkInfo] =
    TestOp.client { _[SecurityFunctions].getOneSharedLink(linkId).call() }

  def getSharedLinkURL(link: TOID): TestOp[String] =
    TestOp.client { _[SecurityFunctions].getSharedLinkURL(link).call() }

  // Higher-level functions

  def createRole(name: String, perms: TID*): TestOp[TID] = {
    for {
      std <- getStd
      role <- makeThing(std.security.customRoleModel, name, std.security.rolePermissionsProp :=> perms.toList)
    }
      yield role
  }

  def getPersonInfoFor(user: TestUser): TestOp[PersonInfo] = {
    for {
      std <- getStd
      userId <- ClientState.userId(user)
      // Note that we grant the role to the *Person*, not the *User*. So we need to fetch the Person records, and find
      // the desired one:
      membersAndInvitees <- getMembers
      members = membersAndInvitees._1
    }
      // TODO: wow, we actually have no good way of finding this! We have the user, and are trying to find the right person.
      // The connection is via Identity, but we don't have that on either side. Should we provide a way to get the
      // current user's Person ID in the current Space, as a better way to do this, instead of comparing display names?
      yield members.find(_.person.displayName == user.display).getOrElse(throw new Exception(s"Couldn't find Space member '${user.base}' to grant Role to!"))
  }

  def grantRoleTo(user: TestUser, role: TID): TestOp[Unit] = {
    for {
      std <- getStd
      personInfo <- getPersonInfoFor(user)
      roles = personInfo.roles
      newRoles = (roles.toSet + role).toList
      _ <- changeProp(personInfo.person.oid, std.security.personRolesProp :=> newRoles)
    }
      yield ()
  }

  def revokeRoleFrom(user: TestUser, role: TID): TestOp[Unit] = {
    for {
      std <- getStd
      personInfo <- getPersonInfoFor(user)
      roles = personInfo.roles
      newRoles = (roles.toSet - role).toList
      _ <- changeProp(personInfo.person.oid, std.security.personRolesProp :=> newRoles)
    }
      yield ()
  }
}

object SecurityMidFuncs extends SecurityMidFuncs
