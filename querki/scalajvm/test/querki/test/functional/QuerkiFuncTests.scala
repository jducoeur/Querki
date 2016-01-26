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
    go to "http://www.google.com/"
    quit()
  }
}
