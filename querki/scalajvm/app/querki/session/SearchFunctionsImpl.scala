package querki.session

import querki.globals._

import querki.api.SearchFunctions
import SearchFunctions._

trait SearchFunctionsImpl extends SessionApiImpl with SearchFunctions {

  def ClientApi:querki.api.ClientApi
  lazy val Search = interface[querki.search.Search]
  
  def search(q:String):Option[SearchResults] = {
    val rawResultsOpt = Search.search(rc, q)
    rawResultsOpt.map { rawResults =>
      val transformed = rawResults.results.map { result =>
        SearchResult(
          ClientApi.thingInfo(Some(result.thing), rc).get,
          result.prop.displayName,
          result.score,
          result.text,
          result.positions
        )
      }
      SearchResults(q, transformed)
    }
  }
}
