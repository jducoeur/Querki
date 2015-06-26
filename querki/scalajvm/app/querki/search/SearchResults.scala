package querki.search

import models._

/**
 * A single result from searching. These results are *not* HTML-neutered, so they must be escaped
 * before rendering!
 */
case class SearchResultInternal(thing:Thing, prop:Property[_,_], score:Double, text:String, positions:List[Int])

case class SearchResultsInternal(request:String, results:Seq[SearchResultInternal])