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
    * @return Nothing, but the State is set up.
    */
  def setupUserAndSpace(usernameBase: String, spaceName: String): TestOp[Unit] = {
    for {
      std <- getStd
      user = TestUser(usernameBase)
      loginResults <- newUser(user)
      space <- createSpace(spaceName)

    }
      yield ()
  }

  def inviteIntoSpace(owner:TestUser, spaceId: TID, member: TestUser): TestOp[Unit] = {
    for {
      _ <- ClientState.switchToUser(owner)
      // TODO: refactor this invite-to-space code into SetupFuncs:
      token <- EmailTesting.nextEmail
      inviteResponse <- SecurityMidFuncs.invite(Seq(member.email), Seq.empty)
      _ = inviteResponse.newInvites should contain (member.display)
      spaceInfo <- TestOp.fetch(_.client.spaceOpt.get)

      _ <- ClientState.switchToUser(member)
      inviteHash <- EmailTesting.extractInviteHash(token)
      _ <- acceptInvite(inviteHash, spaceInfo)
    }
      yield ()
  }
}
