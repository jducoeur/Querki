package querki.test.mid

import org.scalactic.source.Position
import querki.globals._
import org.scalatest.Assertions._

/**
 * Common functions for using in tests.
 */
trait CoreFuncs {
  /**
   * Temporary spewage, during debugging.
   */
  def spew(msg: => Any) = TestOp.pure { QLog.spew(msg.toString) }
  
  /**
   * The name of a test "step", always printed.
   */
  def step(msg: String) = TestOp.pure { QLog.info(s"**** $msg") }

  /**
   * TestOp-friendly version of assert().
   */
  def testAssert(f: => Boolean)(implicit pos: Position): TestOp[Unit] = TestOp.pure { assert(f) }
}

object CoreFuncs extends CoreFuncs
