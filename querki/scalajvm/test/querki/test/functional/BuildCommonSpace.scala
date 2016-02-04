package querki.test.functional

object CommonSpace extends TSpace(
  "Common Space"
)

/**
 * An unconstrained Tag.
 */
object SimpleTag extends TProp(
  "Simple Tag",
  TTagType(None)
)

/**
 * A Model that just has a SimpleTag Property.
 */
object ModelWithTag extends TInstance("Model with Tag")

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
    run(state,
      createSpace(CommonSpace),
      
      designAModel(ModelWithTag)
    )
  }
}
