package querki.test.mid

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

import play.api.mvc.Session

import AllFuncs._

object MainUser extends TestUser("mainuser")

/**
 * This is the primary mid-level test suite. It wraps up tons of other tests into a single run,
 * including the main regression tests. This is mainly for speed: we want to minimize the number
 * of times that we have to start and stop the engine.
 * 
 * The individual components of this should generally have their own tests, usually marked as
 * @Slow, so that we can test them individually.
 * 
 * Note that, unlike the full Functional Tests, this one doesn't work nearly as hard to maintain
 * a consistent sense of "current client state", since we mostly don't care -- at the API level,
 * it is totally straightforward to have a lot of separate sessions going.
 */
class FullMidTests extends MidTestBase
{
  "The system" should {
    "smoketest fully" in {
      val mainSpaceName = "Main Space"
      
      val stateIO = for {
        _ <- initState
        _ <- step("Initialize the test")
        std <- getStd()
        
        _ <- step("Setup the main User")
        loginResults <- newUser(MainUser)
                
        _ <- step("Create the main Space for general testing")
        // mainSpace is the result of the createSpace() function...
        mainSpace <- createSpace(mainSpaceName)
        // ... createdSpaceOpt is what has actually been placed in the TestState...
        createdSpaceOpt <- TestOp.fetch(_.client.spaceOpt)
        _ = createdSpaceOpt.map(_.displayName) must be (Some(mainSpaceName))
        // ... and the Space is also now in the World State:
        createdInWorldOpt <- TestOp.fetch(_.world.spaces.get(mainSpace))
        _ = createdInWorldOpt.map(_.info.displayName) must be (Some(mainSpaceName))
        
        _ <- step("Create the first simple Thing")
        simpleThingId <- makeThing(std.basic.simpleThing, "First Thing")
        simpleThing <- WorldState.fetchThing(simpleThingId)
        _ = simpleThing.info.displayName must be ("First Thing")
        
        _ <- step("Create the first simple Model")
        modelId <- makeModel("First Model")
        model <- WorldState.fetchThing(modelId)
        _ = model.info.displayName must be ("First Model")
        
        _ <- step(s"Create and use the first Property")
        propId <- makeProperty("First Property", exactlyOne, textType)
        _ <- addAndSetProperty(modelId, propId, "Default value of First Property")
        _ <- checkPropValue(modelId, propId, "Default value of First Property")
      }
        yield ()
        
      // At the very end of time, we do this to actually run the test. Note that we have to
      // use unsafeRunSync(), to make sure that we don't shut down the test environment before
      // everything is finished running.
      val ioa = stateIO.run(initialState())
      ioa.unsafeRunSync()
    }
  }
}
