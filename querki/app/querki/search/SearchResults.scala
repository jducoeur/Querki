package querki.search

import models._

case class SearchResult(thing:Thing, prop:Property[_,_], score:Double, text:String, positions:List[Int])

case class SearchResults(request:String, results:Seq[SearchResult])