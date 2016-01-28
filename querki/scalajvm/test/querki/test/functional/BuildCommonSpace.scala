package querki.test.functional

object CommonSpace extends TSpace(
  "Common Space"
)

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
  val buildCommonSpace = TestDef(Some(Admin1), IndexPage, "Build the Common Space") { state =>
    createSpace(CommonSpace)(state)
  }
}
