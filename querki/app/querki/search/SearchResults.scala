package querki.search

import models._

case class SearchResult(thing:Thing, prop:Property[_,_], score:Double, text:DisplayText)

case class SearchResults(request:String, results:Seq[SearchResult])