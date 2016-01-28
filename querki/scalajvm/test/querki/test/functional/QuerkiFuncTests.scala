package querki.test.functional

import org.scalatest._
import org.scalatest.tags.Slow
import org.scalatest.selenium._
import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._

import querki.globals._

/**
 * Self-trait that the elements of the cake should typically use to access all the utilities.
 * 
 * Note that we mix this in, *not* QuerkiFuncTests itself, to avoid circular dependency hell.
 * This keep recompiles down to a manageable level.
 */
trait FuncMixin 
  extends WordSpec 
  with Matchers 
  with org.scalatest.concurrent.Eventually 
  with WebBrowser 
  with OneBrowserPerTest
  with OneServerPerTest
  with FuncDB 
  with FuncData 
  with FuncUtil

/**
 * The root of Querki's Functional Tests.
 * 
 * To run these tests, say `ftst` from sbt / Activator.
 * 
 * 
 * ==Structure==
 * 
 * This is currently defined as a gigantic cake. That's because (for reasons discussed below)
 * we're currently running as one huge, long, functional test. That's not ideal -- it's
 * not parallelizable, in particular -- but it's easier in many ways. So we're going to
 * work this way for the time being, with an expectation of refactoring later when we have
 * the resources. (Note, though, that we first need to figure out how to completely exclude
 * the functional tests -- not even opening a browser window -- when running unit tests. So
 * far, I don't have a solution.) 
 * 
 * 
 * ==Prerequisites for running these tests==
 * 
 * You must first download the native ChromeDriver from
 * 
 *   [[https://sites.google.com/a/chromium.org/chromedriver/downloads]]
 *   
 * Install that on your path, such as /usr/bin/chromedriver, and make sure it has
 * permissions 755. ("which chromedriver" should find it.) Or point the system
 * property "webdriver.chrome.driver" to wherever that is installed.
 * 
 * Also, make sure you have a reasonably *current* version of Chrome installed.
 * 
 * If ScalaTest cancels these tests, saying it "was unable to create a Selenium
 * ChromeDriver on this platform", it means the underlying ChromeDriver threw an
 * exception. Uncomment the DriverTests below and use those to find out what that
 * exception was in order to debug it. The source code for ChromeDriver can be found at:
 * 
 *   [[https://github.com/SeleniumHQ/selenium/tree/master/java/client/src/org/openqa/selenium/chrome]]
 * 
 *   
 * In order to run these tests headless, make sure that you have xvfb installed, with:
 * {{{
 *   sudo apt-get install xvfb
 * }}}
 * In your shell, say:
 * {{{
 *   Xvfb :1 -screen 5 1280x1024x8 &
 * }}}
 * (Note the capital "X" there.) That creates display 1, screen 5 as a virtual framebuffer.
 * 
 * In the shell say:
 * {{{
 *   export DISPLAY=:1.5
 * }}}
 * That tells the system to use this virtual screen for the output. Then start up
 * sbt and run your test.
 * 
 * 
 * These tests assume that you have empty databases named "test_user" and "test_system", that can be
 * accessed by the same user credentials as the usual system and user DBs.
 * 
 * They also assume that you have a local DB named "test_system_template", which is the empty template
 * version of the system DB. You can load this from test_system_template.sql in Querki's git root.
 * 
 * 
 * ==Notes and Future Plans==
 * 
 * In a perfect world, we should be using One[Server|Browser]PerSuite. Problem is, ScalaTest's exclusion
 * mechanism works on *tests*, not *suites*. So even if we are excluding this suite using its tags, it
 * still starts up both the server and the browser, though we don't want them. So instead, we're structuring
 * this as one huge test. It's suboptimal, but adequate for now.
 * 
 * 
 * @author jducoeur
 */
@Slow
class QuerkiFuncTests 
  // Infrastructure mix-ins, from ScalaTest and Play:
  extends WordSpec
  with Matchers
  with OneServerPerTest
  with OneBrowserPerTest
  // For now, we're just going to target Chrome. Eventually, obviously, we should
  // test this stuff cross-browser:
  with ChromeFactory
  with WebBrowser
  
  // Structural mix-ins for the tests:
  with FuncDB
  with FuncData
  with FuncUtil
  with FuncMixin
{
  /**
   * This is where we override the standard Application settings.
   */
  override def newAppForTest(td:TestData) = {
    FakeApplication(
      additionalConfiguration = Map(
        // For the moment, the names of the test DBs are hardcoded. That will probably have to
        // change eventually.
        "db.system.url" -> "jdbc:mysql://localhost/test_system?characterEncoding=UTF-8",
        "db.user.url" -> "jdbc:mysql://localhost/test_user?characterEncoding=UTF-8"
      )
    )
  }
  
  "I should be able to open a web browser" in {
    setupDatabase()
    
    // Starting point: go to the Querki root page, not yet logged in.
    // 19001 is the default port used for Play Functional Testing. We can and probably
    // should change at at some point, but it's fine for now:
    go to "http://localhost:19001/"
    
    loginAs(admin1)
    
    quit()
  }
}

// Uncomment this and use it if ScalaTest is claiming that it "was unable to create a
// Selenium ChromeDriver on this platform". Unfortunately, ScalaTest turns *all* exceptions
// into that error, suppressing ChromeDriver's often-helpful exceptions.
//
//class DriverTests
//  extends WordSpec
//  with Matchers
//  with ChromeFactory
//{
//  "I should be able to get a decent error" in {
//    import org.openqa.selenium.chrome.ChromeDriver
//    
//    System.setProperty("webdriver.chrome.driver", "/usr/bin/chromedriver")
//    new ChromeDriver()
//  }
//}
