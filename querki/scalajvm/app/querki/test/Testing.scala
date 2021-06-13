package querki.test

import querki.globals._

/**
 * This is a special ecot, which does nothing at runtime, but provides a bit of introspection capability during
 * unit tests.
 */
trait Testing {

  /**
   * Declares that some event happened.
   *
   * Code should insert this as needed *by the tests* to introspect on side-effects that are not otherwise
   * observable.
   *
   * Note that the parameter is intentionally by-name, so this call is basically zero-code at runtime. Put whatever
   * data structure is needed by the tests as the event -- since it is only constructed at unit-test time, you don't
   * need to worry much about the cost.
   *
   * @param event a data structure that describes what happened
   */
  def happened(event: => Any): Unit
}

class TestingEcot(e: Ecology) extends QuerkiEcot(e) with Testing {

  /**
   * In the normal runtime environment, this does absolutely nothing.
   */
  def happened(event: => Any): Unit = {}
}
