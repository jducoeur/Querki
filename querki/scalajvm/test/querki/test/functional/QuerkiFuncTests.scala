package querki.test.functional

import org.scalatest._
import org.scalatest.tags.Slow
import org.scalatest.selenium._
import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._

import querki.globals._

/**
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
    // Tell the underlying ChromeDriver where to find the native implementation
    // TODO: this really belongs in a documented system property instead?
    System.setProperty("webdriver.chrome.driver", "/usr/bin/chromedriver")
    go to "http://www.google.com/"
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
