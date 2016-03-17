package querki.search

import scala.concurrent.Future

import querki.api.{SpaceApiImpl, AutowireParams}
import querki.globals._

class SearchFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with SearchFunctions {
  
  import SearchFunctions._

  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Search = interface[querki.search.Search]
  
  def doRoute(req:Request):Future[String] = route[SearchFunctions](this)(req)
  
  def search(q:String):Future[Option[SearchResults]] = {
    implicit val s = state
    val rawResultsOpt = Search.search(q)
    rawResultsOpt match { 
      case Some(rawResults) => {
        val transformedFuts = rawResults.results.map { result =>
          ClientApi.thingInfo(result.thing, rc) map { tInfo =>
            SearchResult(
              tInfo,
              result.score,
              result.elements.map { elem =>
                SearchResultElement(
                  elem.prop.displayName,
                  elem.score,
                  elem.text,
                  elem.positions)
              }
            )
          }
        }
        Future.sequence(transformedFuts) map { transformed => Some(SearchResults(q, transformed)) }
      }
      case None => Future.successful(None)
    }
  }
}
