package querki.security

import org.scalatest.tags.Slow
import org.scalatest.Matchers._
import querki.data.TID
import querki.test.mid._
import AllFuncs._
import ClientState.withUser
import SecurityMidFuncs._
import org.scalactic.source.Position

object SecurityMidTests {
  val securityTests: TestOp[Unit] = {
    for {
      _ <- regressionTestQIbu6oeej
    }
      yield ()
  }

  /**
    * This (and the two convenience functions after it) are the easiest way to check read visibility of a particular
    * Thing for the specified user.
    */
  def checkNameRealityFor(shouldBeReal: Boolean, name: String, who: TestUser)(implicit position: Position): TestOp[Unit] = {
    for {
      check <- withUser(who) { getThingInfo(TID(name)) }
      _ = assert(check.isTag != shouldBeReal, s"Expected $name to be ${if (shouldBeReal) "real" else "tag"}, and it wasn't!")
    }
      yield ()
  }
  def checkNameIsRealFor(name: String, who: TestUser)(implicit position: Position)=
    checkNameRealityFor(true, name, who)
  def checkNameIsMissingFor(name: String, who: TestUser)(implicit position: Position) =
    checkNameRealityFor(false, name, who)
  
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
      _ <- inviteIntoSpace(owner, space, member)

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

  lazy val regressionTestQIbu6of67: TestOp[Unit] = {
    val ownerName = "bu6of67 Owner"
    val owner = TestUser(ownerName)
    val memberName = "bu6of67 Member"
    val member = TestUser(memberName)
    val spaceName = "bu6of67 Space"

    for {
      _ <- step("Regression test for QI.bu6of67")
      std <- getStd
      _ <- newUser(owner)
      space <- createSpace(spaceName)

      _ <- newUser(member)

      // Set up the Space and members:
      _ <- inviteIntoSpace(owner, space, member)

      // Build the Things:
      _ <- ClientState.switchToUser(owner)
      model <- makeModel("The Model")
      instanceName = "The Instance"
      instance <- makeThing(model, instanceName)

      // Check that the member can see them:
      _ <- checkNameIsRealFor(instanceName, member)

      // Prevent the member from seeing the instance:
      perms <- permsFor(model)
      instancePermThing = perms.instancePermThing.get
      _ <- changeProp(instancePermThing, std.security.canReadPerm :=> std.security.owner)
      _ <- checkNameIsMissingFor(instanceName, member)

      // Create the role, and make the instance visible via the role, which should be enough to change anything:
      role <- createRole("Visible Role", std.security.canReadPerm)
      _ <- changeProp(instancePermThing, std.security.canReadPerm :=> role)
      _ <- checkNameIsMissingFor(instanceName, member)

      // Give the role to the member, which *should* make the instance visible. (This is failing, hence the bug.)
      _ <- grantRoleTo(member, role)
      _ <- checkNameIsRealFor(instanceName, member)
    }
      yield ()
  }
}

@Slow
class SecurityMidTests extends MidTestBase {
  import SecurityMidTests._
  
  "Regression tests" should {
    "pass" in {
      runTest(regressionTestQIbu6oeej)
      runTest(regressionTestQIbu6of67)
    }
  }
}
