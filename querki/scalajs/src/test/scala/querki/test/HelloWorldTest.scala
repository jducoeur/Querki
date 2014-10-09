package querki.test

import utest._

object HelloWorldTest extends TestSuite {
  def tests = TestSuite {
    'Hello {
      assert(1 == 1)
    }
  }
}
