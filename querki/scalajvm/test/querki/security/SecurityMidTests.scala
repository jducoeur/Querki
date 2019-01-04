package querki.security

import org.scalatest.tags.Slow
import org.scalatest.Matchers._

import querki.test.mid._
import AllFuncs._

object SecurityMidTests {
  val securityTests: TestOp[Unit] = {
    for {
      _ <- regressionTestQIbu6oeej
    }
      yield ()
  }
  
  lazy val regressionTestQIbu6oeej: TestOp[Unit] = {
    val ownerName = "bu6oeej Owner"
    val owner = TestUser(ownerName)
    val memberName = "bu6oeej Member"
    val member = TestUser(memberName)
    val spaceName = "bu6oeej Space"
    
    for {
      _ <- step("Regression test for QI.bu6oeej")
      std <- getStd
      _ <- newUser(owner)
      space <- createSpace(spaceName)
      
      _ <- newUser(member)
      
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

@Slow
class SecurityMidTests extends MidTestBase {
  import SecurityMidTests._
  
  "QI.bu6oeej" should {
    "pass" in {
      runTest(regressionTestQIbu6oeej)
    }
  }
}
