package querki.test.mid

/**
 * Primary tests of the signup/login/logout sequence.
 */
class LoginTests extends MidTestBase with LoginFuncs {
  "A new user" should {
    "be able to sign up, login and logout" in {
      val user = TestUser("simpleuser")
      signup(user)
      
      val session = login(user).session
      session("username") must be (user.handle)
      
      val loggedOutSession = logout(session)
      loggedOutSession.get("username") must be (None)
    }
  }
}
