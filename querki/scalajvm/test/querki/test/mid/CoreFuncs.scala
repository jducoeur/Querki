package querki.test.mid

import querki.globals._

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
}

object CoreFuncs extends CoreFuncs
