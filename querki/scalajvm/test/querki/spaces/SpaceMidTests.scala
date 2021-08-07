package querki.spaces

import org.scalatest.tags.Slow
import org.scalatest.Matchers._
import querki.data.TID
import querki.test.mid._
import AllFuncs._

object SpaceMidTests {

  /**
   * Test the ability to clone a Thing.
   */
  lazy val testQI7w4gdpl: TestOp[Unit] = {
    for {
      _ <- step("Regression test for QI.7w4gdpl")
      owner <- setupUserAndSpace("QI7w4gdpl Owner", "QI7w4gdpl Space")

      model <- makeModel("The Model")
      propId <- makeProperty("The Property", exactlyOne, textType)
      instance <- makeThing(model, "The Instance", propId :=> "The Initial Value")

      clone1Text <- evaluateQL(instance, """_copyThing() -> _oid""")
      clone1 = TID(clone1Text.plaintext)
      _ <- checkPropValue(clone1, propId, "The Initial Value")

      clone2Text <- evaluateQL(instance, """_copyThing(The Property(""Different Value"")) -> _oid""")
      clone2 = TID(clone2Text.plaintext)
      _ <- checkPropValue(clone2, propId, "Different Value")
    } yield ()
  }
}

@Slow
class SpaceMidTests extends MidTestBase {
  import SpaceMidTests._

  "SpaceMidTests" should {
    "pass" in {
      runTest(testQI7w4gdpl)
    }
  }
}
