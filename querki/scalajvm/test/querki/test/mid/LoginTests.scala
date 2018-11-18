package querki.test.mid

import org.scalatest.tags.Slow

/**
 * Primary tests of the signup/login/logout sequence.
 */
@Slow
class LoginTests extends MidTestBase with LoginFuncs with ClientFuncs {
  "A new user" should {
    "be able to sign up, login and logout" in {
      val user = TestUser("simpleuser")
      val signupSession = signup(user).session
      validateSignup(user)(signupSession)
      
      val session = login(user).session
      session("username") must be (user.handle)
      
      val loggedOutSession = logout(session)
      loggedOutSession.get("username") must be (None)
    }
  }
}
