package querki.test.mid

import org.scalatest.tags.Slow

import play.api.mvc.Session

/**
 * Primary tests of the signup/login/logout sequence.
 */
@Slow
class LoginTests extends MidTestBase with LoginFuncs with ClientFuncs {
  "A new user" should {
    "be able to sign up, login and logout" in {
      val user = TestUser("simpleuser")
      val testOp = for {
        loginResults <- newUser(user)
        _ = loginResults.session("username") must be (user.handle)
        sess <- logout
        _ = sess.get("username") must be (None)
      }
        yield ()
        
      val ioa = testOp.run(TestState.empty)
      ioa.unsafeRunSync()
    }
  }
}