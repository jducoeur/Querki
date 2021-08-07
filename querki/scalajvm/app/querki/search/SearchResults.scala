package querki.search

import models._

/**
 * A single Property that matches the search term
 *
 * @param prop The Property that contained the match.
 * @param text The value of that Property, which contained the match.
 * @param score The quality of the match, as a value between 0 and 1.
 * @param positions The places where the match was found within the text.
 */
case class SearchResultElementInternal(
  prop: AnyProp,
  text: String,
  score: Double,
  positions: List[Int]
)

/**
 * A single result from searching. These results are *not* HTML-neutered, so they must be escaped
 * before rendering!
 *
 * @param thing The Thing that had matches in it.
 * @param score The overall quality of matching this Thing to this search query. This is theoretically
 *   a composite of the scores of the elements, but that's pretty ad hoc at this point.
 * @param elements The Properties that actually matched.
 */
case class SearchResultInternal(
  thing: Thing,
  score: Double,
  elements: List[SearchResultElementInternal]
)

case class SearchResultsInternal(
  request: String,
  results: Seq[SearchResultInternal]
)
