package querki.test.regression

import querki.globals._
import querki.test._

/**
 * This is a catch-all for miscellaneous regression tests that don't fit neatly into
 * other buckets.
 *
 * @author jducoeur
 */
class RegressionTests extends QuerkiTests {
  // See https://querki.net/u/jducoeur/alpha-issue-tracking/#.3y28a3r
  ".3y28a3r" should {
    "be fixed" in {
      class TSpace extends CommonSpace {
        val myModel = new SimpleTestThing("My Model", singleLinkProp(sandbox))
        val t1 = new TestThing("Thing 1", myModel)
        val t2 = new TestThing("Thing 2", myModel)
        val t3 = new TestThing("Thing 3", myModel)
        val t4 = new TestThing("Thing 4", myModel)
      }
      implicit val s = new TSpace

      // This should return empty; the bug was that it wasn't. Note that an essential part of
      // the recipe is that this query is running in the context of the Space, not the Model:
      pql("""[[My Model._instances -> _filter(false) -> Single Link]]""") should
        equal("")
    }
  }

  ".3y28a3v" should {
    "be fixed" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, QList, "Num List")
        val plusOne = new TestProperty(Basic.QLType, ExactlyOne, "Plus One")

        val testThing = new SimpleTestThing("My Thing", numProp(3, 4, 5), plusOne("_plus(1)"))
      }
      implicit val s = new TSpace

      // TEMP:
//      turnOnContextLogging()

      pqlt(s.testThing, """[[Num List -> Plus One -> _commas]]""") should
        equal("4, 5, 6")
    }
  }
}
