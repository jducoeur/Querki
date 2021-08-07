package querki.test.functional

/**
 * This is the first of probably many regression-test suites.
 *
 * @author jducoeur
 */
trait RegressionTests1 { this: FuncMixin with BuildCommonSpace =>

  object regression1 extends TestDef(Some(Admin1), AnyPage, "Regression Suite 1")({ state => state }) {

    override def subTests = Seq(
      // .3y28an1: apostrophes getting double-escaped in page titles:
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28an1") { state =>
        val thingName = "Don't I work?"
        val testThing = TInstance(thingName)
        val s2 = createAnyThing(testThing)(state)
        pageTitle should be(thingName)
        s2
      },
      // .3y28auu: Anonymous shouldn't see the Design and Create menu picks as active:
      //
      // Changed on 7/7/16 -- these menu picks should no longer be *visible* for Anonymous, since
      // they are in Easy mode:
      TestDef(None, LoginPage, ".3y28auu") { state =>
        run(
          state,
          // We need to go directly to the target Space, since we're Anonymous:
          goTo(CommonSpace),
          s {
            openMenuFor(RefreshItem)

            find(DesignModelItem.id) should be(empty)
            find(CreateThingItem.id) should be(empty)
          }
        )
      },
      // .3y28asq: using ____ in the Page Header for the Model of an unrefied Tag doesn't work right:
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28asq") { state =>
        val tagText = "Tag for 3y28asq"

        object TagModel extends TInstance("Model for Tag 3y28asq")
        val SimpleTag = TTestProp("Tag 3y28asq", QSet, TTagType, extras = Seq(RestrictedToModel(TagModel)))

        object ModelWithTag extends TInstance("Model with Tag 3y28asq")

        object InstanceWithSimpleTag extends TInstance("Instance 3y28asq", model = ModelWithTag)

        run(
          state,
          designAModel(
            TagModel,
            addExistingProperty(PageHeaderProp),
            PageHeaderProp.setValue("### {{testHeader:____}}")
          ),
          // Create a Model with a Tag Property:
          designAModel(ModelWithTag, createProperty(SimpleTag)),
          // Create an Instance and set the Tag:
          createAnyThing(InstanceWithSimpleTag, SimpleTag.setValue(tagText)),
          // We should now be looking at the Thing. Click on the link:
          { state =>
            waitFor(linkText(tagText))
            click.on(linkText(tagText))
            val page = TagPage(tagText)
            waitForTitle(page)
            waitForRendered()
            // This is the bit that fails in this bug: the text inside testHeader is wrong:
            find(className("testHeader")).get.text should include(tagText)
            state -> page
          }
        )
      },
      // .3y28aq5: the header for tags is coming out as ".-1":
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28aq5") { state =>
        val tagText = "Tag for 3y28aq5"

        object SimpleTag extends TTestProp[TTagType.type]("Tag 3y28aq5", QSet, TTagType)

        object ModelWithTag extends TInstance("Model with Tag 3y28aq5")

        object InstanceWithSimpleTag extends TInstance("Instance 3y28aq5", model = ModelWithTag)

        run(
          state,
          // Create a Model with a Tag Property:
          designAModel(ModelWithTag, createProperty(SimpleTag)),
          // Create an Instance and set the Tag:
          createAnyThing(InstanceWithSimpleTag, SimpleTag.setValue(tagText)),
          // We should now be looking at the Thing. Click on the link:
          { state =>
            waitFor(linkText(tagText))
            click.on(linkText(tagText))
            val page = TagPage(tagText)
            // This is where the test fails on this bug -- the title is wrong:
            waitForTitle(page)
            state -> page
          }
        )
      },
      // .3y28ahw: when you edit the Name in the Advanced Editor, the "Done" button takes you to a Tag
      // of the old Name instead of to the new Thing:
      TestDef(Some(Admin1), RootPage(CommonSpace), ".3y28ahw") { state =>
        object TheModel extends TInstance("Model for .3y28ahw")

        val newName = "Adjusted Model for .3y28ahw"

        run(
          state,
          // Create the Model -- Name gets set automatically:
          designAModel(TheModel),
          // Now edit it and change the name. It should fail when trying to leave the
          // editor, because it won't find the correct name or OID:
          editModel(
            NameProp.setValue(newName),
            // TODO: this is a horrible hack. The above setValue() should do this automatically:
            { (tt, state) =>
              val thing = tt.asInstanceOf[TInstance].copy(display = newName)
              state.updateSpace(space => space.copy(things = space.things + (thing.tid -> thing)))
            }
          )
        )
      }
    )
  }
}
