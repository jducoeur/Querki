package querki.search

import scala.concurrent.Future

import upickle.default.{macroRW, ReadWriter => RW}

import querki.data.ThingInfo

trait SearchFunctions {
  import SearchFunctions._

  /**
   * Search for all occurrences of the given string.
   */
  def search(q: String): Future[Option[SearchResults]]
}

object SearchFunctions {

  case class SearchResultElement(
    propName: String,
    score: Double,
    text: String,
    positions: List[Int]
  )

  object SearchResultElement {
    implicit val rw: RW[SearchResultElement] = macroRW
  }

  /**
   * A single result from searching. The text is *not* HTML-neutered, so it must be escaped
   * before rendering!
   */
  case class SearchResult(
    thing: ThingInfo,
    score: Double,
    elements: List[SearchResultElement]
  )

  object SearchResult {
    implicit val rw: RW[SearchResult] = macroRW
  }

  case class SearchResults(
    request: String,
    results: Seq[SearchResult]
  )

  object SearchResults {
    implicit val rw: RW[SearchResults] = macroRW
  }
}
