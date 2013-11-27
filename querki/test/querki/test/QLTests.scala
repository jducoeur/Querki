package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

class QLTests extends QuerkiTests {
  // === Attachments ===
  "Photos" should {
    "self-render when linked" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Photo]]""") should 
        equal ("""![My Photo](a/My-Photo)""")
    }
  }
}