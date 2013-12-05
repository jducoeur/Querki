package querki.test

import models.system.{DisplayNameProp}

import querki.util.PublicException

class PropertyTests extends QuerkiTests {
  "Display Name Property" should {
    "require a length of at least 1" in {
      DisplayNameProp.validate("hello!", commonState)
      DisplayNameProp.validate("a", commonState)
      val ex = intercept[PublicException] {
        DisplayNameProp.validate("", commonState)
      }
      assert(ex.msgName == "Types.Text.tooShort")
    }
  }
}