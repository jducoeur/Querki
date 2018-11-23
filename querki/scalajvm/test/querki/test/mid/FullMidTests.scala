package querki.test.mid

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
class FullMidTests
  extends MidTestBase
  with ClientFuncs
  with LoginFuncs
  with SpaceFuncs
{
  "The system" should {
    "smoketest fully" in {
      step("Set up the main User")
      val signupSession = signup(MainUser).session
      validateSignup(MainUser)(signupSession)
      implicit val mainSession = login(MainUser).session
      
      val mainSpace = createSpace("Main Space")
      
    }
  }
}
