package querki.pages

import scala.concurrent.{Future, Promise}

import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.jquery._

import utest._
import utest.ExecutionContext.RunNow
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.SearchFunctions
import SearchFunctions._
import querki.data.ThingInfo
import querki.test._
import querki.util.ScalatagUtils

trait JQueryEventCreator extends js.Object {
  def Event(name:String):JQueryEventObject = ???
}
object JQueryEventCreator {
  implicit def jq2EventCreator(jqs:JQueryStatic):JQueryEventCreator =
    jqs.asInstanceOf[JQueryEventCreator]
}
import JQueryEventCreator._

object SearchResultsTests extends ThingPageTests {

  /**
   * This will execute the given trigger code (which should cause a Page change), 
   * and returns a Future that will be fulfilled once the resulting Page is fully
   * loaded. 
   */
  def afterPageChange(trigger: => Unit):Future[dom.HTMLDivElement] = {
    val promise = Promise[dom.HTMLDivElement]
    PageManager.nextChangeFuture.map { page =>
      page.renderedContentFuture.map { content =>
        promise.success(content)
      }
    }
    trigger
    promise.future
  }

  def tests = TestSuite {
    "The SearchGadget invokes the page and fetches the results" - {
      
      // We just start with an empty ThingPage, since we don't actually care about it: 
      setupPage(
        div()
      )
      
      val query1 = "sand"
      val result1 = SearchResult(
          ThingInfo(".sandbox", Some("Sandbox"), "Sandbox", ".simpleThing", models.Kind.Thing, false, true, true, false, false),
          "Display Name",
          .75,
          "Sandbox",
          List(0))
      val result2 = SearchResult(
          ThingInfo(".sandbox2", Some("Sandbox-2"), "Sandbox 2", ".simpleThing", models.Kind.Thing, false, true, true, false, false),
          "Display Name",
          .70,
          "Sandbox 2",
          List(0))
      val result3 = SearchResult(
          ThingInfo(".anotherThing", Some("Another-Thing"), "Another Thing", ".simpleThing", models.Kind.Thing, false, true, true, false, false),
          "Default View",
          .60,
          "A random sandbox, containing sand.",
          List(9, 29))
          
      val query2 = "floobity"
      
      registerApiHandler[SearchFunctions]("search")(new SearchFunctions with AutowireHandler {
        def search(q:String):Option[SearchResults] = {
          if (q == query1) {
            Some(SearchResults(q, Seq(result3, result1, result2)))
          } else if (q == query2) {
            None
          } else {
            assert(q != s"Error: SearchResultsTests got unexpected query $q")
            None
          }
        }
    
        def handle(request:Core.Request[String]):Future[String] = route[SearchFunctions](this)(request)
      })    
      
      // This is a def, since it will change as we switch pages:
      def searchInput = $(".search-query")
      def resultHeader = $("._searchResultHeader")
      
      val contentFut = afterPageChange
        {
          searchInput.value(query1)
          val triggerEvent = $.Event("keydown")
          triggerEvent.which = 13
          searchInput.trigger(triggerEvent)
        }
      
      contentFut.map { content =>
        assert(resultHeader.text == s"""Found 3 matches for "$query1"""")
        val results = $("._searchResult")
        assert(results.length == 3)
        // result3 should be at index 2, and should contain 2 highlighted sections:
        val complexResult = results.get(2)
        val highlights = $(complexResult).find(".searchResultHighlight")
        assert(highlights.length == 2)
      }
    }
  }
}
