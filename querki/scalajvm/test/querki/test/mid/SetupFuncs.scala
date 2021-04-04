package querki.test.mid

import querki.data._
import AllFuncs._
import org.scalatest.Matchers._
import querki.security.SecurityMidFuncs

/**
 * Functions that provide common setup, to reduce boilerplate for special-purpose tests.
 */
trait SetupFuncs {

  /**
   * This creates the specified normal user, and has them create a Space.
   *
   * @param usernameBase The base of this user's name. This must be unique, so should be based on the suite name.
   *                     Note that this is the display name, and may be arbitrarily long.
   * @param spaceName The name of the Space, which only needs to be unique per-User.
   * @return The owner of the Space, and the TID of the Space.
   */
  def setupUserAndSpace(
    usernameBase: String,
    spaceName: String
  ): TestOp[TestUser] = {
    for {
      std <- getStd
      user = TestUser(usernameBase)
      loginResults <- newUser(user)
      space <- createSpace(spaceName)
    } yield user
  }

  case class RegressionInfo(
    owner: TestUser,
    std: StdThings,
    spaceId: TID
  )

  /**
   * High-level setup function to initialize things for a typical regression test.
   *
   * This is purely a convenience function, but extracts the pattern that I seem to use repeatedly.
   *
   * @param ticketId the base OID for the ticket governing this test, like "7w4gerb"
   */
  def setupStandardRegressionTest(ticketId: String): TestOp[RegressionInfo] = {
    for {
      _ <- step(s"Regression test for QI.$ticketId")
      std <- getStd
      owner <- setupUserAndSpace(s"QI$ticketId Owner", s"QI$ticketId Space")
      spaceId <- TestOp.fetch(_.curSpace.info.oid)
    } yield RegressionInfo(owner, std, spaceId)
  }

  def fetchSpaceInfo(): TestOp[SpaceInfo] = TestOp.fetch(_.client.spaceOpt.get)

  def inviteIntoSpace(
    owner: TestUser,
    spaceId: TID,
    member: TestUser
  ): TestOp[Unit] = {
    val doIt = for {
      _ <- ClientState.switchToUser(owner)
      // TODO: refactor this invite-to-space code into SetupFuncs:
      token <- EmailTesting.nextEmail
      inviteResponse <- SecurityMidFuncs.invite(Seq(member.email), Seq.empty)
      _ = inviteResponse.newInvites should contain(member.display)
      spaceInfo <- fetchSpaceInfo()

      _ <- ClientState.switchToUser(member)
      inviteHash <- EmailTesting.extractInviteHash(token)
      _ <- acceptInvite(inviteHash, spaceInfo)
      _ <- setClientSpace(spaceInfo)
    } yield ()

    ClientState.cachingUser(doIt)
  }
}
