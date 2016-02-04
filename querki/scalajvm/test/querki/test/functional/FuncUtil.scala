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
  
  /**
   * You can create recursively-nested "suites" by overriding subTests(). The subTests will be run
   * *after* the guts of the higher-level test.
   */
  def subTests:Seq[TestDef] = Seq.empty
}

/**
 * Common utility operations for the tests.
 * 
 * @author jducoeur
 */
trait FuncUtil extends FuncMenu with FuncEditing { this:FuncMixin =>
  
  /**
   * Note that this implicit is available pretty much throughout the mixins.
   */
  implicit val browser:org.scalatest.selenium.WebBrowser = this
  
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
    def msg(name:String, params:(String, String)*) = msgs(page).msg(name, params:_*)
  }
  
  /**
   * Waits until the element with the specified id actually exists
   */
  def waitFor(id:String) = {
    eventually { find(id) should not be empty }
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
    waitForTitle(page.msg("pageTitle", page.titleParams:_*))
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
    state.copy(currentUserOpt = Some(user)) -> IndexPage
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
    // TBD: why do we need this eventually? Without it, we seem to have a race condition under xvfb, where we are
    // often getting the title, but _thingOID isn't yet displayed. That's disturbingly weird -- AFAIK, they're
    // synchronous with each other? If we hit more instances of this, we may need to add something magical to the
    // end of the rendering process, that sets a signal we can consistently wait on.
    eventually { find("_thingOID") should not be empty }
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
    state.copy(spaces = state.spaces + (createdSpace.tid -> createdSpace), currentSpace = Some(createdSpace)) -> RootPage(space)
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
    if (test.desiredPage is state.currentPage) {
      state
    } else {
      test.desiredPage match {
        
        case IndexPage => {
          click on "_index_button"
          waitForTitle(IndexPage)
          state -> IndexPage
        }
        
        case LoginPage => fail("We appear to be looking for the LoginPage; should have gotten there by changing users?")
        
        case RootPage(space) => {
          // We want to be at the root of this specific Space
          def rootThroughIndex() = {
            click on "_index_button"
            waitForTitle(IndexPage)
            click on linkText(space.display)
            waitForTitle(test.desiredPage)
          }
          
          state.currentSpace match {
            // Are we already in the desired Space?
            case Some(curSpace) if (curSpace is space) => {
              // Yes, so just click on the spaceLink:
              click on "_spaceLink"
              waitForTitle(test.desiredPage)
            }
            // Otherwise, go to the Index:
            case _ => rootThroughIndex()
          }
          
          state -> test.desiredPage
        }
        
        case _ => fail(s"adjustPage tried to go to ${test.desiredPage}, and we're not ready for that yet.")
      }
    }
  }
  
  /**
   * The top-level test runner, which runs through all the specified tests, starting with the given state.
   * 
   * Tests are frequently composed of Ops, but do not have to be.
   */
  def runTests(tests:TestDef*)(initialState:State):State = {
    (initialState /: tests) { (state, test) =>
      val stateWithUser:State = adjustUser(state, test)
      val stateWithPage = adjustPage(stateWithUser, test)
      spew(s"Running test ${test.desc}")
      val topResult = test.run(stateWithPage)
      // If this test has subTests, recurse into them:
      runTests(test.subTests:_*)(topResult)
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
