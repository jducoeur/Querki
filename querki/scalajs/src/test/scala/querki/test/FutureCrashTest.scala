package querki.test

import scala.concurrent.Future

import utest._

/**
 * This test has nothing to do with Querki per se -- it's just a minimized test for a bad
 * interaction between the Scala.js compiler and uTest.
 */
object FutureCrashTest extends TestSuite {

  def spewing[T](msg:String)(f: => T):T = {
    try {
      val result = f
      result
    } catch {
      case ex:Exception => { println(s"  $msg failed: $ex"); ex.printStackTrace(); throw ex }
    }
  }
  implicit val queue = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

  def tests = TestSuite {
    "Crash the compiler when I spew a future" - {
      spewing("Nothing much") {
        val fut = Future { 1 }
      }
//      println("This prevents the crash")
    }
  }
}
