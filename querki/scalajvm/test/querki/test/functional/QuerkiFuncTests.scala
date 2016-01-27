package querki.test.functional

import org.scalatest._
import org.scalatest.tags.Slow
import org.scalatest.selenium._
import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._

import querki.globals._

/**
 * The root of Querki's Functional Tests.
 * 
 * IMPORTANT PREREQUISITES: you must first download the native ChromeDriver from
 * 
 *   https://sites.google.com/a/chromium.org/chromedriver/downloads
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
 *   https://github.com/SeleniumHQ/selenium/tree/master/java/client/src/org/openqa/selenium/chrome
 * 
 *   
 * In order to run these tests headless, make sure that you have xvfb installed, with:
 * 
 *   sudo apt-get install xvfb
 *   
 * In your shell, say:
 * 
 *   Xvfb :1 -screen 5 1280x1024x8 &
 *   
 * (Note the capital "X" there.) That creates display 1, screen 5 as a virtual framebuffer.
 * 
 * In the shell say:
 * 
 *   export DISPLAY=:1.5
 *   
 * That tells the system to use this virtual screen for the output. Then start up
 * sbt and run your test.
 * 
 * @author jducoeur
 */
@Slow
class QuerkiFuncTests 
  extends WordSpec
  with Matchers
  with BeforeAndAfterAll
  with OneServerPerSuite
  with OneBrowserPerSuite
  // For now, we're just going to target Chrome. Eventually, obviously, we should
  // test this stuff cross-browser:
  with ChromeFactory
  with WebBrowser
{
  "I should be able to open a web browser" in {
    // 19001 is the default port used for Play Functional Testing. We can and probably
    // should change at at some point, but it's fine for now:
    go to "http://localhost:19001/"
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
