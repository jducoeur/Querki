package querki.pages

import scala.concurrent.{Future, Promise}

import scala.scalajs.js

import org.scalajs.dom
import org.querki.jquery._
import org.querki.jsext._

import utest._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.data.{TID, ThingInfo}
import querki.search.SearchFunctions
import SearchFunctions._
import querki.test._
import querki.util.ScalatagUtils

object SearchResultsTests extends ThingPageTests {
  def tests = TestSuite {
    "The SearchGadget invokes the page and fetches the results" - {
      
      // We just start with an empty ThingPage, since we don't actually care about it: 
      setupPage(
        div()
      ) flatMap { page:Page =>
        val query1 = "sand"
        val result1 = SearchResult(
          ThingInfo(TID(".sandbox"), Some("Sandbox"), "Sandbox", TID(".simpleThing"), models.Kind.Thing, false, true, true, false, false, None),
          "Display Name",
          .75,
          "Sandbox",
          List(0))
        val result2 = SearchResult(
          ThingInfo(TID(".sandbox2"), Some("Sandbox-2"), "Sandbox 2", TID(".simpleThing"), models.Kind.Thing, false, true, true, false, false, None),
          "Display Name",
          .70,
          "Sandbox 2",
          List(0))
        val result3 = SearchResult(
          ThingInfo(TID(".anotherThing"), Some("Another-Thing"), "Another Thing", TID(".simpleThing"), models.Kind.Thing, false, true, true, false, false, None),
          "Default View",
          .60,
          "A random sandbox, containing sand.",
          List(9, 29))
          
        val query2 = "floobity"
      
        registerApiHandler[SearchFunctions]("search")(new SearchFunctions with AutowireHandler {
          def search(q:String):Future[Option[SearchResults]] = {
            Future.successful(if (q == query1) {
              Some(SearchResults(q, Seq(result3, result1, result2)))
            } else if (q == query2) {
              None
            } else {
              assert(q != s"Error: SearchResultsTests got unexpected query $q")
              None
            })
          }
    
          def handle(request:Core.Request[String]):Future[String] = route[SearchFunctions](this)(request)
        })    
      
        // This is a def, since it will change as we switch pages:
        def searchInput = $(".search-query")
        def resultHeader(implicit content:Page) = $(content.elem).find("._searchResultHeader")
        def results(implicit content:Page) = $(content.elem).find("._searchResult")
      
        def enterSearchTerm(term:String) = {
          searchInput.value(term)
          val triggerEvent = $.Event("keydown", JQueryEventObject.which(13))
          searchInput.trigger(triggerEvent)        
        }
        
        /**
         * This submits the search queries, and after each one, checks the contents of the resulting page:
         */
        val fut = for {
          query1Page <- afterPageChange { enterSearchTerm(query1) }
          dummy1 = {
            implicit val content = query1Page
            assert(resultHeader.text == s"""Found 3 matches for "$query1"""")
            assert(results.length == 3)
            // result3 should be at index 2, and should contain 2 highlighted sections:
            val complexResult = results.get(2)
            val highlights = $(complexResult).find(".searchResultHighlight")
            assert(highlights.length == 2)
          }
          query2Page <- afterPageChange { enterSearchTerm(query2) }
          dummy2 = {
            implicit val content = query2Page
            assert(resultHeader.text == s"""Nothing found for "$query2"""")
            assert(results.length == 0)
          }
        }
          yield query2Page
          
        fut.withTimeout
      }
    }
  }
}
