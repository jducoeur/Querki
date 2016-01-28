package querki.test.functional

/**
 * This test mixin has tests that construct the "common space", which we will use for a bunch of
 * other tests.
 * 
 * @author jducoeur
 */
trait BuildCommonSpace { this:FuncMixin =>
  /**
   * This operation creates the Common Space itself.
   */
  val createCommonSpace = TestDef(Some(admin1), IndexPage) { state =>
    // TODO:
    state
  }
}