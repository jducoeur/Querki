package querki.security

import org.scalatest.tags.Slow
import org.scalatest.Matchers._

import querki.data.TID
import querki.test.mid._
import AllFuncs._
import SecurityMidFuncs._

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

      // Set up the Space and members:
      _ <- ClientState.switchToUser(owner)
      // TODO: refactor this invite-to-space code into SetupFuncs:
      token <- EmailTesting.nextEmail
      inviteResponse <- SecurityMidFuncs.invite(Seq(member.email), Seq.empty)
      _ = inviteResponse.newInvites should contain (member.display)
      spaceInfo <- TestOp.fetch(_.client.spaceOpt.get)
      
      _ <- ClientState.switchToUser(member)
      inviteHash <- EmailTesting.extractInviteHash(token)
      _ <- acceptInvite(inviteHash, spaceInfo)

      // Build the Things:
      _ <- ClientState.switchToUser(owner)
      parent <- makeModel("Parent Model")
      parentInstanceName = "Parent Instance"
      parentInstance <- makeThing(parent, parentInstanceName)
      child <- makeModel(parent, "Child Model")
      childInstanceName = "Child Instance"
      childInstance <- makeThing(child, childInstanceName)

      // Check that the Things are okay -- if isTag is false, that means it is a real Thing:
      _ <- ClientState.switchToUser(member)
      parentCheck1 <- getThingInfo(TID(parentInstanceName))
      _ = parentCheck1.isTag should be (false)
      childCheck1 <- getThingInfo(TID(childInstanceName))
      _ = childCheck1.isTag should be (false)

      // Change the security on the *child*. This is the heart of the bug: it was actually fetching the Parent's
      // Instance Permissions Thing:
      _ <- ClientState.switchToUser(owner)
      // We need to fetch the parent's perms first, to set up the bug:
      parentPerms <- permsFor(parent)
      childPerms <- permsFor(child)
      _ = assert(childPerms.instancePermThing.isDefined, "Model lacking an instancePermThing!")
      instancePermThing = childPerms.instancePermThing.get
      _ <- changeProp(instancePermThing, std.security.canReadPerm :=> std.security.owner)

      // Now, the member should still be able to see the parent, but not the child:
      _ <- ClientState.switchToUser(member)
      childCheck2 <- getThingInfo(TID(childInstanceName))
      _ = childCheck2.isTag should be (true)
      // The bug resulted in the *parent's* instances not being visible:
      parentCheck2 <- getThingInfo(TID(parentInstanceName))
      _ = parentCheck2.isTag should be (false)
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
