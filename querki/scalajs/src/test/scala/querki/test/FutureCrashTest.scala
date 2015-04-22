package querki.test

import scala.concurrent.Future

import utest._

/**
 * This test has nothing to do with Querki per se -- it's just a minimized test for a bad
 * interaction between the Scala.js compiler and uTest. See utest Issue #70.
 */
/*
object FutureCrashTest extends TestSuite {

  def wrapping[T](f: => T):T = {
    f
  }
  implicit val queue = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

  def tests = TestSuite {
    "Crash the compiler when I spew a future" - {
      wrapping { val fut = Future { 1 } }
      println("This prevents the crash")
    }
  }
}
*/
