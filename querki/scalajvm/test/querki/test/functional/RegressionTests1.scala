package querki.test.functional

/**
 * This is the first of probably many regression-test suites.
 * 
 * @author jducoeur
 */
trait RegressionTests1 { this:FuncMixin with BuildCommonSpace =>
  object regression1 extends TestDef(Some(Admin1), AnyPage, "Regression Suite 1")({ state => state }) {
    override def subTests = Seq(
        
      // .3y28an1: apostrophes getting double-escaped in page titles:
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28an1") { state =>
        val thingName = "Don't I work?"
        val testThing = TInstance(thingName)
        val s2 = createAnyThing(testThing)(state)
        pageTitle should be (thingName)
        s2
      },
      
      // .3y28aq5: the header for tags is coming out as ".-1":
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28aq5") { state =>
        
        val tagText = "Tag for 3y28aq5"
        
        object SimpleTag extends TProp[TTagType.type]("Tag 3y28aq5", TSet, TTagType)

        object ModelWithTag extends TInstance("Model with Tag 3y28aq5")
        
        object InstanceWithSimpleTag extends TInstance("Instance 3y28aq5",
          model = ModelWithTag
        )
        
        run(state,
          // Create a Model with a Tag Property:
          designAModel(ModelWithTag,
            createProperty(SimpleTag)),
            
          // Create an Instance and set the Tag:
          createAnyThing(InstanceWithSimpleTag,
            SimpleTag.setValue(tagText)),
            
          // We should now be looking at the Thing. Click on the link:
          { state =>
            waitFor(linkText(tagText))
            click on linkText(tagText)
            val page = TagPage(tagText)
            // This is where the test fails on this bug -- the title is wrong:
            waitForTitle(page)
            state -> page
          }
        )
      }
    )
  }
}
