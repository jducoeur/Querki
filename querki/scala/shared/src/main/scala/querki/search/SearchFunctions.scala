package querki.search

import querki.data.ThingInfo

trait SearchFunctions {
  import SearchFunctions._
  /**
   * Search for all occurrences of the given string.
   */
  def search(q:String):Option[SearchResults]
}

object SearchFunctions {
  /**
   * A single result from searching. The text is *not* HTML-neutered, so it must be escaped
   * before rendering!
   */
  case class SearchResult(thing:ThingInfo, propName:String, score:Double, text:String, positions:List[Int])

  case class SearchResults(request:String, results:Seq[SearchResult])
}
