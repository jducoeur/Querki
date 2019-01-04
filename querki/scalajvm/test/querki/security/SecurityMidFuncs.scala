package querki.security

import autowire._

import org.scalatest.Matchers._

import querki.data._
import querki.globals._
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
}

object SecurityMidFuncs extends SecurityMidFuncs
