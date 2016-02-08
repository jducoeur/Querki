package querki.test.functional

/**
 * Search-centric tests.
 * 
 * @author jducoeur
 */
trait Search { this:FuncMixin =>
  object SearchableSpace extends TSpace(
    "Searchable Space"
  )
  
  object Sandbox1 extends TInstance("Sandbox 1")
  object Sandbox2 extends TInstance("Sandbox 2")

  /**
   * This operation creates the Common Space itself.
   */
  val runSearchTests = TestDef(Some(Admin1), IndexPage, "Run Search Tests") { state =>
    run(state,
      // Set up a couple of simple Things to search for:
      createSpace(SearchableSpace),
      createAnyThing(Sandbox1),
      createAnyThing(Sandbox2),
      
      // Okay, now try searching from the main search box:
      { state =>
        val term = "sand"
        
        // We need to be a little explicit about how we do our entry, so we can hit Enter:
        click on className("_searchInput")
        enter(term)
        pressKeys("\uE007")
        val searchPage = Search(term) 
        waitFor(searchPage)
        
        // Check that the header is correct:
        find(className("_searchResultHeader")).get.text should 
          include (searchPage.msg("resultsHeader", ("numFound" -> "2"), ("query" -> term)))
        
        state
      }
    )
  }
}