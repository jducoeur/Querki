package querki.security

import autowire._
import querki.data._
import querki.globals.execContext
import querki.test.mid._
import SecurityFunctions._
import AllFuncs._
import org.scalactic.source.Position
import org.scalatest.Matchers._
import querki.test.mid.ClientState.withUser
import querki.test.mid.EmailTesting.inviteHashPre

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

  def getMyInfo(): TestOp[Option[PersonInfo]] =
    TestOp.client { _[SecurityFunctions].getMyInfo().call() }

  def invite(
    emails: Seq[String],
    collabs: Seq[TID]
  ): TestOp[InviteResponse] =
    TestOp.client { _[SecurityFunctions].invite(emails, collabs).call() }

  def archiveThisSpace(): TestOp[Boolean] =
    TestOp.client { _[SecurityFunctions].archiveThisSpace().call() }

  def permsFor(thing: TID): TestOp[ThingPermissions] =
    TestOp.client { _[SecurityFunctions].permsFor(thing).call() }

  def getOnePerm(id: TID): TestOp[PermInfo] =
    TestOp.client { _[SecurityFunctions].getOnePerm(id).call() }

  def getAllPerms(): TestOp[Seq[PermInfo]] =
    TestOp.client { _[SecurityFunctions].getAllPerms().call() }

  def getSharedLinksForRole(roleId: TOID): TestOp[Seq[SharedLinkInfo]] =
    TestOp.client { _[SecurityFunctions].getSharedLinksForRole(roleId).call() }

  def getOneSharedLink(linkId: TOID): TestOp[SharedLinkInfo] =
    TestOp.client { _[SecurityFunctions].getOneSharedLink(linkId).call() }

  def getSharedLinkURL(link: TOID): TestOp[String] =
    TestOp.client { _[SecurityFunctions].getSharedLinkURL(link).call() }

  def removeFromSpace(people: TID*): TestOp[Boolean] =
    TestOp.client { _[SecurityFunctions].removeFromSpace(people).call() }

  // Higher-level functions

  def createRole(
    name: String,
    perms: TID*
  ): TestOp[TID] = {
    for {
      std <- getStd
      role <- makeThing(std.security.customRoleModel, name, std.security.rolePermissionsProp :=> perms.toList)
    } yield role
  }

  def getPersonInfoFor(user: TestUser)(implicit pos: Position): TestOp[PersonInfo] = {
    for {
      std <- getStd
      personInfoOpt <- withUser(user) {
        for {
          info <- getMyInfo()
        } yield info
      }
    } yield personInfoOpt.getOrElse(throw new Exception(s"No Person found for '${user.base}!'"))
  }

  def grantRoleTo(
    user: TestUser,
    role: TID
  ): TestOp[Unit] = {
    for {
      std <- getStd
      personInfo <- getPersonInfoFor(user)
      roles = personInfo.roles
      newRoles = (roles.toSet + role).toList
      _ <- changeProp(personInfo.person.oid, std.security.personRolesProp :=> newRoles)
    } yield ()
  }

  def revokeRoleFrom(
    user: TestUser,
    role: TID
  ): TestOp[Unit] = {
    for {
      std <- getStd
      personInfo <- getPersonInfoFor(user)
      roles = personInfo.roles
      newRoles = (roles.toSet - role).toList
      _ <- changeProp(personInfo.person.oid, std.security.personRolesProp :=> newRoles)
    } yield ()
  }

  /**
   * This mimics what happens in EditShareableInvite.
   */
  def createShareableLinkForRole(
    role: TID,
    name: String,
    requiresMembership: Boolean,
    isOpenInvite: Boolean
  ): TestOp[SharedLinkInfo] = {
    for {
      std <- getStd
      inviteThing <-
        makeThing(
          std.security.sharedInviteModel,
          name,
          std.security.inviteRoleLink :=> role.underlying,
          std.security.inviteRequiresMembership :=> requiresMembership,
          // TBD: EditShareableInvite has another clause here that I don't entirely understand:
          std.security.isOpenInvite :=> isOpenInvite
        )
      linkInfo <- getOneSharedLink(TOID(inviteThing.underlying))
    } yield linkInfo
  }

  /**
   * Given a URL from [[getSharedLinkURL()]], this extracts the bit we care about.
   */
  def extractOpenInviteHash(link: String): String = {
    val hashPos = link.indexOf(inviteHashPre) + inviteHashPre.length
    link.substring(hashPos)
  }

  def setInstancePermsFor(
    thing: TID,
    perm: ThingInfo,
    to: ThingInfo
  ): TestOp[Unit] = {
    for {
      perms <- permsFor(thing)
      instancePermThing = perms.instancePermThing.get
      _ <- changeProp(instancePermThing, perm :=> to)
    } yield ()
  }

  def setToMembersOnly(thing: TID): TestOp[Unit] = {
    getStd.flatMap { std =>
      setInstancePermsFor(thing, std.security.canReadPerm, std.security.members)
    }
  }

  /**
   * This (and the two convenience functions after it) are the easiest way to check read visibility of a particular
   * Thing for the specified user.
   */
  def checkNameRealityFor(
    shouldBeReal: Boolean,
    name: String,
    who: TestUser
  )(implicit
    position: Position
  ): TestOp[Unit] = {
    for {
      check <- withUser(who) { getThingInfo(TID(name)) }
      _ = assert(
        check.isTag != shouldBeReal,
        s"Expected $name to be ${if (shouldBeReal) "real" else "tag"} for ${who.display}, and it wasn't!"
      )
    } yield ()
  }

  def checkNameIsRealFor(
    name: String,
    who: TestUser
  )(implicit
    position: Position
  ) =
    checkNameRealityFor(true, name, who)

  def checkNameIsMissingFor(
    name: String,
    who: TestUser
  )(implicit
    position: Position
  ) =
    checkNameRealityFor(false, name, who)

}

object SecurityMidFuncs extends SecurityMidFuncs
