package querki.pages

/**
 * This is simply an enumeration of all of the page types, so that the Server and Client can
 * coordinate about them.
 */
object PageIDs {
  type PageID = Int

  val ThingPage = 1
  val SearchResultsPage = 2
}
