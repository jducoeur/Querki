package querki.test.mid

import org.scalatest.Matchers._
import org.scalatest.tags.Slow

import AllFuncs._

object BasicMidTests {
  /**
   * The most essential smoketests, for setting up a trivial Space.
   */
  val basicTests: TestOp[Unit] = {
    for {
      _ <- step("Basic smoketest suite")
      std <- getStd
      basicUser = TestUser("Basic Test User")
      basicSpaceName = "Basic Test Space"
      
      _ <- step("Setup the User")
      loginResults <- newUser(basicUser)

      _ <- step("Create the main Space for general testing")
      // mainSpace is the result of the createSpace() function...
      mainSpace <- createSpace(basicSpaceName)
      // ... createdSpaceOpt is what has actually been placed in the TestState...
      createdSpaceOpt <- TestOp.fetch(_.client.spaceOpt)
      _ = createdSpaceOpt.map(_.displayName) should be (Some(basicSpaceName))
      // ... and the Space is also now in the World State:
      createdInWorldOpt <- TestOp.fetch(_.world.spaces.get(mainSpace))
      _ = createdInWorldOpt.map(_.info.displayName) should be (Some(basicSpaceName))
      
      _ <- step("Create the first simple Thing")
      simpleThingId <- makeThing(std.basic.simpleThing, "First Thing")
      simpleThing <- WorldState.fetchThing(simpleThingId)
      _ = simpleThing.info.displayName should be ("First Thing")
      
      _ <- step("Create the first simple Model")
      modelId <- makeModel("First Model")
      model <- WorldState.fetchThing(modelId)
      _ = model.info.displayName should be ("First Model")
      
      _ <- step(s"Create and use the first Property")
      propId <- makeProperty("First Property", exactlyOne, textType)
      _ <- addAndSetProperty(modelId, propId, "Default value of First Property")
      _ <- checkPropValue(modelId, propId, "Default value of First Property")
      
      _ <- step(s"Create an Instance, and mess with it")
      instanceId <- makeThing(modelId, "First Instance")
      _ <- checkPropValue(instanceId, propId, "Default value of First Property")
      _ <- changeProp(instanceId, propId :=> "Instance value")
      _ <- checkPropValue(instanceId, propId, "Instance value")
    }
      yield ()
  }
}

@Slow
class BasicMidTests extends MidTestBase {
  "The system" should {
    "basically work" in {
      runTest(BasicMidTests.basicTests)
    }
  }
}
