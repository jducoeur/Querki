package querki.test.functional

/**
 * This is the first of probably many regression-test suites.
 * 
 * @author jducoeur
 */
trait RegressionTests1 { this:FuncMixin =>
  object regression1 extends TestDef(Some(Admin1), AnyPage, "Regression Suite 1")({ state => state }) {
    override def subTests = Seq(
        
      // .3y28an1: apostrophes getting double-escaped in page titles:
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28an1") { state =>
        val thingName = "Don't I work?"
        val testThing = TInstance(thingName)
        val s2 = createAnyThing(testThing)(state)
        pageTitle should be (thingName)
        s2
      }
    )
  }
}
