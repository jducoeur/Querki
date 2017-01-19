package querki.test.functional

import scala.collection.JavaConverters._

import org.openqa.selenium.WebElement

import querki.util.QLog

/**
 * Common utility operations for the tests.
 * 
 * @author jducoeur
 */
trait FuncUtil extends FuncData with FuncMenu with FuncEditing with FuncTypes with FuncProps with FuncPages { this:FuncMixin =>

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
    def runTest(state:State) = test(state)
    
    /**
     * You can create recursively-nested "suites" by overriding subTests(). The subTests will be run
     * *after* the guts of the higher-level test.
     */
    def subTests:Seq[TestDef] = Seq.empty
  }

  /**
   * Note that this implicit is available pretty much throughout the mixins.
   */
  implicit val browser:org.scalatest.selenium.WebBrowser = this
  
  /**
   * This works around the very unfortunate limitation that WebBrowser.Element doesn't allow
   * searching *within* it, even though the underlying WebDriver does. Note that, due to
   * ScalaTest being a little over-eager about private and sealed, we can't return an Element
   * here; instead, we have to return a raw Selenium WebElement.
   */
  implicit class EnhancedElement(elem:Element) {
    def find(q:Query):Option[WebElement] = {
      try {
        Some(elem.underlying.findElement(q.by))
      } catch {
        case e:NoSuchElementException => None
      }
    }
    
    def findAll(q:Query):Iterable[WebElement] = {
      elem.underlying.findElements(q.by).asScala
    }
  }
  
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
    def title = msg("pageTitle", page.titleParams:_*)
  }
  
  def trying[T](msg: => String)(f: => T):T = {
    try {
      f
    } catch {
      case ex:Exception => {
        spew(msg)
        ex.printStackTrace()
        throw ex
      }
    }
  }
  
  /**
   * Waits until the element with the specified id actually exists
   */
  def waitFor(q:Query):Unit = {
    trying(s"Query $q was never satisfied on page $pageTitle") {
      eventually { find(q) should not be empty }
    }
  }
  def waitFor(idStr:String):Unit = waitFor(id(idStr))
  
  /**
   * This waits until the page title (IMPORTANT: *not* the header, the title in the tab) matches
   * the given string.
   * 
   * In general, favor the version of this that takes a Page when possible.
   */
  def waitForTitle(str:String):Unit = {
    trying(s"Failed in waitForTitle -- looking for $str, but it is at $pageTitle") {
      eventually { pageTitle should be (str) }
    }
  }
  
  def waitForTitle(page:QPage):Unit = {
    waitForTitle(page.title)
  }
  
  def waitForRendered():Unit = {
    waitFor("_pageRendered")
  }
  
  def waitFor(page:QPage):Unit = {
    waitForTitle(page)
    waitForRendered()
  }
  
  def waitForStatus(msg:String):Unit = {
    eventually { find("statusText") should not be empty }
    find("statusText").map(_.text) should be (Some(msg))
  }
  
  def goToSpaceRoot(space:TSpace)(state:State):State = {
    waitFor("_spaceLink")
    click on "_spaceLink"
    val page = RootPage(space)
    waitFor(page)
    state -> page
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
    clickMenuItem(LogoutItem)
    // Note: the login page isn't yet part of the Client, so it doesn't yet use clientStrings:
    waitForTitle("Welcome to Querki")
    spew(s"Logged out")
    state.copy(currentUserOpt = None) -> LoginPage
  }
  
  def waitForThing[T <: TThing[T]](t:T)(state:State):State = {
    waitForTitle(t.display)
    // TBD: why do we need this eventually? Without it, we seem to have a race condition under xvfb, where we are
    // often getting the title, but _thingOID isn't yet displayed. That's disturbingly weird -- AFAIK, they're
    // synchronous with each other? If we hit more instances of this, we may need to add something magical to the
    // end of the rendering process, that sets a signal we can consistently wait on.
    eventually { find("_thingOID") should not be empty }
    state -> ThingPage(t)
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
    waitForRendered()
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
    // After creating the Space, we get taken to the Edit Space Info Page:
    waitForTitle(EditSpaceInfoPage)
    // TODO: we should actually play with the settings here. We should set the Summary and Details,
    // and play with access control, and confirm that those work as expected. But note that doing so
    // will probably require embedding _thingOID into the Edit Space Info page, and doing the below
    // waitUntilCreated stuff and rebuilding state *before* editing those fields.
    click on "_spaceInfoDoneButton"
    val createdSpace = waitUntilCreated(space).copy(url = currentUrl)
    val spaceThing = TInstance(createdSpace.display, createdSpace.tid)
    val fullSpace = createdSpace + spaceThing
    state.copy(spaces = state.spaces + (fullSpace.tid -> fullSpace), currentSpace = Some(fullSpace)) -> RootPage(fullSpace)
  }
  
  /**
   * Go directly to this Space, without intervening clicking. This is faster but less realistic,
   * so use it sparingly.
   */
  def goTo(space:TSpace)(state:State):State = {
    val realSpace = state.getSpace(space)
    go to realSpace.url
    val page = RootPage(realSpace) 
    waitFor(page)
    state -> page
  }
  
  def adjustUser(state:State, test:TestDef):State = {
    trying(s"Failed when trying to switch user from ${state.currentUserOpt} to ${test.desiredUser}") {
      if (test.desiredUser != state.currentUserOpt) {
        test.desiredUser match {
          case Some(targetUser) => {
            // We want to be logged-in as targetUser. Logout if we're logged in:
            val s = state.currentUserOpt match {
              case Some(currentUser) => logout(state)
              case None => {
                if (state.currentPage is LoginPage)
                  state
                else {
                  // We're not logged in, but also not at the login page. So click the index
                  // button to get there:
                  waitFor("_index_button")
                  click on "_index_button"
                  waitForTitle(LoginPage)
                  state -> LoginPage
                }
              }
            }
            loginAs(targetUser)(s)
          }
          // We don't want to be logged-in, so log out:
          case None => logout(state)
        }
      } else
        state
    }
  }
  
  def adjustPage(state:State, test:TestDef):State = {
    trying (s"Failed while trying to switch page from ${state.currentPage} to ${test.desiredPage}") {
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
              waitFor("_index_button")
              click on "_index_button"
              waitForTitle(IndexPage)
              click on linkText(space.display)
              waitForTitle(test.desiredPage)
            }
            
            state.currentSpace match {
              // Are we already in the desired Space?
              case Some(curSpace) if (curSpace matches space) => {
                // Yes, so just click on the spaceLink:
                waitFor("_spaceLink")
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
  }
  
  /**
   * The top-level test runner, which runs through all the specified tests, starting with the given state.
   * 
   * Tests are frequently composed of Ops, but do not have to be.
   */
  def runTests(tests:TestDef*)(initialState:State):State = {
    (initialState /: tests) { (state, test) =>
      spew(s"Running test ${test.desc}")
      val stateWithUser:State = adjustUser(state, test)
      val stateWithPage = adjustPage(stateWithUser, test)
      val topResult = test.runTest(stateWithPage)
      // If this test has subTests, recurse into them:
      val finalResult = runTests(test.subTests:_*)(topResult)
      spew(s"Completed test ${test.desc}")
      finalResult
    }
  }
  
  /**
   * Trivial wrapper, for using non-ops inside of run().
   */
  def s(f: => Unit)(state:State):State = {
    f
    state
  }
  
  /**
   * Runs a sequence of Ops -- functions that take the current State, do something, and return a new State.
   * 
   * As a general rule, functions that take additional parameters beyond the State should be curried, with
   * the State at the end, so that you can feed them into here.
   */
  def run(initialState:State, ops:State => State*):State = {
    (initialState /: ops) { (state, op) =>
      trying (s"Failed while running an operation in State $state") {
        op(state)
      }
    }
  }
}
