package querki.test.functional

import querki.util.QLog

/**
 * The definition of a test case.
 * 
 * These are the high-level, relatively decoupled tests. Each one declares its preconditions, and
 * it is up to the runTests() engine to get things to the right preconditions before the test
 * runs. Keep in mind that that's not magic: you mustn't point to a user or page that doesn't yet
 * exist in the State.
 * 
 * @param desiredUser The User who needs to be logged in before this test begins.
 * @param desiredPage The Page that should be showing before this test begins. Should usually
 *   be either the index Page, or a specific Page in a Space that is known to the State.
 * @param test The actual test function.
 */
case class TestDef(desiredUser:Option[TestUser], desiredPage:QPage, desc:String)(test:State => State)
{
  def run(state:State) = test(state)
}

/**
 * Common utility operations for the tests.
 * 
 * @author jducoeur
 */
trait FuncUtil { this:FuncMixin =>
  
  /**
   * This loads the clientStrings and parses them, so we can compare against what we expect.
   */
  lazy val messages = new FuncMessages(app).messages
  
  /**
   * Access the messages for a specific page.
   */
  def pageMsgs(pageName:String) = messages.getPackage("pages").getPackage(pageName)
  def msgs(page:QPage):Messages = pageMsgs(page.name)
  
  implicit class PageWithMessages(page:QPage) {
    def msg(name:String) = msgs(page).msg(name)
  }
  
  /**
   * This waits until the page title (IMPORTANT: *not* the header, the title in the tab) matches
   * the given string.
   * 
   * In general, favor the version of this that takes a Page when possible.
   */
  def waitForTitle(str:String):Unit = {
    eventually { pageTitle should be (str) }
  }
  
  def waitForTitle(page:QPage):Unit = {
    waitForTitle(page.msg("pageTitle"))
  }
  
  /**
   * Spew some output, so that we can see what's going on. This is especially useful when you're
   * running headless, which is the most common approach.
   */
  def spew(str: => String) = {
    // TODO: make this config-selectable
    QLog.spew(str)
  }
  
  def loginAs(user:TestUser)(state:State):State = {
    textField("name").value = user.handle
    pwdField("password").value = user.password
    click on "login_button"
    waitForTitle(IndexPage)
    spew(s"Logged in as ${user.display}")
    state -> IndexPage
  }
  
  def logout(state:State):State = {
    click on "logout_button"
    // Note: the login page isn't yet part of the Client, so it doesn't yet use clientStrings:
    waitForTitle("Welcome to Querki")
    spew(s"Logged out")
    state -> LoginPage
  }
  
  /**
   * After we're done creating a Thing, this waits until the ThingPage appears, fetches its TID, and
   * returns the adjusted Thing.
   * 
   * This may not work if PageHeader is set!
   */
  def waitUntilCreated[T <: TThing[T]](t:T):T = {
    waitForTitle(t.display)
    val tid = find("_thingOID").map(_.text).getOrElse(fail(s"Couldn't find the OID on-page for ${t.display}"))
    spew(s"Created ${t.display} as $tid")
    t.withTID(tid)
  }
  
  /**
   * Creates the specified Space.
   * 
   * Precondition: you should already be at the IndexPage.
   */
  def createSpace(space:TSpace)(state:State):State = {
    assert(state.currentPage == IndexPage)
    spew(s"Creating Space ${space.display}")
    click on "_createSpaceButton"
    waitForTitle(CreateSpace)
    textField("_newSpaceName").value = space.display
    click on "_createSpaceButton"
    val createdSpace = waitUntilCreated(space)
    state.copy(spaces = state.spaces :+ createdSpace) -> RootPage(space)
  }
  
  def adjustUser(state:State, test:TestDef):State = {
    if (test.desiredUser != state.currentUserOpt) {
      test.desiredUser match {
        case Some(targetUser) => {
          // We want to be logged-in as targetUser. Logout if we're logged in:
          val s = state.currentUserOpt match {
            case Some(currentUser) => logout(state)
            case None => state
          }
          loginAs(targetUser)(s)
        }
        // We don't want to be logged-in, so log out:
        case None => logout(state)
      }
    } else
      state    
  }
  
  def adjustPage(state:State, test:TestDef):State = {
    if (test.desiredPage != state.currentPage) {
      test.desiredPage match {
        case IndexPage => {
          click on "_index_button"
          waitForTitle(IndexPage)
          state -> IndexPage
        }
        case LoginPage => fail("We appear to be looking for the LoginPage; should have gotten there by changing users?")
        case RootPage(space) => {
          fail("Not yet ready to give Spaces as test prerequisites. Write this!")
        }
        case _ => fail(s"adjustPage tried to go to ${test.desiredPage}, and we're not ready for that yet.")
      }
    } else
      state
  }
  
  /**
   * The top-level test runner, which starts out with an empty world and runs all the high-level tests.
   * 
   * Tests are frequently composed of Ops, but do not have to be.
   */
  def runTests(tests:TestDef*):State = {
    val initialState:State = InitialState
    (initialState /: tests) { (state, test) =>
      val stateWithUser:State = adjustUser(state, test)
      val stateWithPage = adjustPage(stateWithUser, test)
      spew(s"Running test ${test.desc}")
      test.run(stateWithPage)
    }
  }
  
  /**
   * Runs a sequence of Ops -- functions that take the current State, do something, and return a new State.
   * 
   * As a general rule, functions that take additional parameters beyond the State should be curried, with
   * the State at the end, so that you can feed them into here.
   */
  def run(initialState:State, ops:State => State*):State = {
    (initialState /: ops) { (state, op) =>
      op(state)
    }
  }
}
