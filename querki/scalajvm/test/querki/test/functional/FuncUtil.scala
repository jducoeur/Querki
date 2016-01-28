package querki.test.functional

/**
 * The definition of a test case.
 * 
 * @param desiredUser The User who needs to be logged in before this test begins.
 * @param desiredPage The Page that should be showing before this test begins. Should usually
 *   be either the index Page, or a specific Page in a Space that is known to the CurrentState.
 * @param test The actual test function.
 */
case class TestDef(desiredUser:Option[TestUser], desiredPage:Page)(test:CurrentState => CurrentState)
{
  def run(state:CurrentState) = test(state)
}

/**
 * Common utility operations for the tests.
 * 
 * @author jducoeur
 */
trait FuncUtil { this:FuncMixin =>
  def loginAs(state:CurrentState, user:TestUser):CurrentState = {
    textField("name").value = user.handle
    pwdField("password").value = user.password
    click on "login_button"
    eventually { pageTitle should be ("Your Spaces") }
    state -> IndexPage
  }
  
  def logout(state:CurrentState):CurrentState = {
    click on "logout_button"
    eventually { pageTitle should be ("Welcome to Querki") }
    state -> LoginPage
  }
  
  def adjustUser(state:CurrentState, test:TestDef):CurrentState = {
    if (test.desiredUser != state.currentUserOpt) {
      test.desiredUser match {
        case Some(targetUser) => {
          // We want to be logged-in as targetUser. Logout if we're logged in:
          val s = state.currentUserOpt match {
            case Some(currentUser) => logout(state)
            case None => state
          }
          loginAs(s, targetUser)
        }
        // We don't want to be logged-in, so log out:
        case None => logout(state)
      }
    } else
      state    
  }
  
  def adjustPage(state:CurrentState, test:TestDef):CurrentState = {
    if (test.desiredPage != state.currentPage) {
      test.desiredPage match {
        case IndexPage => {
          click on "_index_button"
          eventually { pageTitle should be ("Your Spaces") }
          state -> IndexPage
        }
        case LoginPage => fail("We appear to be looking for the LoginPage; should have gotten there by changing users?")
      }
    } else
      state
  }
  
  /**
   * Runs the specified tests, adjusting as necessary between them.
   */
  def runTests(tests:TestDef*):CurrentState = {
    val initialState = CurrentState(None, LoginPage)
    (initialState /: tests) { (state, test) =>
      val stateWithUser:CurrentState = adjustUser(state, test)
      val stateWithPage = adjustPage(stateWithUser, test)
      test.run(stateWithPage)
    }
  }
}
