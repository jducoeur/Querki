package querki.test.functional

/**
 * Persistence-oriented tests.
 */
trait Persistence { this:FuncMixin with BuildCommonSpace =>
  val runPersistenceTests = TestDef(Some(Admin1), RootPage(CommonSpace), "Test Space reloading") { state =>
    val thingName = "Thing to be reloaded"
    val testThing = TInstance(thingName)
    
    run(state,
      createAnyThing(testThing),
      { s =>
        clickMenuItem(OpenAdvancedItem)
        waitFor(AdvancedCommandsPage)
        click on "_reloadButton"
        waitForStatus("Reloaded")
        s
      },
      goToSpaceRoot(CommonSpace),
      wrap(click on linkText(thingName)),
      waitForThing(testThing)
    )
  }
}
