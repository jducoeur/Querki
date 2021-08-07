package querki.publication

import org.scalatest.tags.Slow
import org.scalatest.Matchers._
import querki.data.TID
import querki.test.mid._
import AllFuncs._
import ClientState.withUser
import PublicationMidFuncs._
import querki.security.SecurityMidFuncs._

object PublicationMidTests {

  lazy val basicPublicationTests: TestOp[Unit] = {
    val ownerName = "BasicPub Owner"
    val owner = TestUser(ownerName)
    val memberName = "BasicPub Member"
    val member = TestUser(memberName)
    val spaceName = "BasicPub Space"

    for {
      _ <- step("Basic Publication Test")
      std <- getStd
      _ <- newUser(owner)
      space <- createSpace(spaceName)

      _ <- newUser(member)

      // Set up the Space and members:
      _ <- inviteIntoSpace(owner, space, member)

      // Build the Things:
      _ <- ClientState.switchToUser(owner)
      model <- makeModel("The Model", std.publication.publishableProp :=> true)
      instanceName = "This Instance"
      instance <- makeThing(model, instanceName)

      // The member shouldn't initially be able to see the instance, since it isn't published yet:
      _ <- checkNameIsMissingFor(instanceName, member)

      // Publish it:
      _ <- publish(instance)

      // Now the member should be able to see it:
      _ <- checkNameIsRealFor(instanceName, member)

      // Now, try the same tests with another instance, publishing via QL instead:
      instance2Name = "Next Instance"
      instance2 <- makeThing(model, instance2Name)
      _ <- checkNameIsMissingFor(instance2Name, member)
      _ <- evaluateQL(instance2, "_publish")
      _ <- checkNameIsRealFor(instance2Name, member)

      // Now create another instance, and demonstrate that the member (who doesn't have the rights) cannot
      // publish it:
      instance3Name = "Third Instance"
      instance3 <- makeThing(model, instance3Name)
      _ <- checkNameIsMissingFor(instance3Name, member)
      _ <- withUser(member) { evaluateQL(instance3, "_publish") }
      _ <- checkNameIsMissingFor(instance3Name, member)
    } yield ()
  }
}

@Slow
class PublicationMidTests extends MidTestBase {
  import PublicationMidTests._

  "Basic Publication Tests" should {
    "pass" in {
      runTest(basicPublicationTests)
    }
  }
}
