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
  
  def afterPageChange(trigger: => Unit):Future[dom.HTMLDivElement] = {
    val promise = Promise[dom.HTMLDivElement]
    PageManager.observePageChanges { (root, page) =>
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
          
      val query2 = "floobity"
      
      registerApiHandler[SearchFunctions]("search")(new SearchFunctions with AutowireHandler {
        def search(q:String):Option[SearchResults] = {
          if (q == query1) {
            Some(SearchResults(q, Seq(result1)))
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
        assert(resultHeader.text == s"""Found 1 matches for "$query1"""")
      }
    }
  }
}
