package querki.test.functional

/**
 * This test mixin has tests that construct the "common space", which we will use for a bunch of
 * other tests.
 * 
 * @author jducoeur
 */
trait BuildCommonSpace { this:FuncMixin =>

  object CommonSpace extends TSpace(
    "Common Space"
  )

  /**
   * This operation creates the Common Space itself.
   */
  val buildCommonSpace = TestDef(Some(Admin1), IndexPage, "Build the Common Space") { state =>
    run(state,
      createSpace(CommonSpace)
    )
  }
}
