package querki.test.functional

/**
 * Search-centric tests.
 *
 * @author jducoeur
 */
trait Search { this: FuncMixin =>

  object SearchableSpace
    extends TSpace(
      "Searchable Space"
    )

  object Sandbox1 extends TInstance("Sandbox 1")
  object Sandbox2 extends TInstance("Sandbox 2")
  object SearchFrom extends TInstance("Search From")

  def searchFor(
    input: Query,
    term: String,
    nExpected: Int
  )(
    state: State
  ): State = {
    spew(s"Searching for $term")
    // We need to be a little explicit about how we do our entry, so we can hit Enter:
    click.on(input)
    enter(term)
    pressKeys("\uE007")
    val searchPage = Search(term)
    waitFor(searchPage)

    // Check that the header is correct:
    if (nExpected > 0)
      find(className("_searchResultHeader")).get.text should
        include(searchPage.msg("resultsHeader", ("numFound" -> nExpected.toString), ("query" -> term)))
    else
      find(className("_searchResultHeader")).get.text should
        include(searchPage.msg("noResultsHeader", ("query" -> term)))

    state -> searchPage
  }

  /**
   * This operation creates the Common Space itself.
   */
  val runSearchTests = TestDef(Some(Admin1), IndexPage, "Run Search Tests") { state =>
    run(
      state,
      // Set up a couple of simple Things to search for:
      createSpace(SearchableSpace),
      createAnyThing(Sandbox1),
      createAnyThing(Sandbox2),
      // Okay, now try searching from the main search box:
      searchFor(className("_searchInput"), "sand", 2),
      // Check that an unsuccessful search works as expected:
      searchFor(className("_searchInput"), "fmarb", 0),
      // Now, add a page with its own search box:
      createAnyThing(SearchFrom, DefaultViewProp.setValue("Here is a search box: [[_searchInput]]")),
      // Now search via this element in the page:
      searchFor(className("_userSearchInput"), "sand", 2)
    )
  }
}
