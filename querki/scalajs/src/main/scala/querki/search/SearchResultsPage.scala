package querki.search

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import querki.globals._

import SearchFunctions._
import querki.display.HookedGadget
import querki.pages.{Page, PageContents, ParamMap}
import scala.scalajs.js.Any.fromFunction1

/**
 * The Gadget in the MenuBar, which initiates a Search. This mostly just leads to the
 * SearchResultsPage.
 */
class SearchGadget(implicit e:Ecology) extends HookedGadget[dom.HTMLInputElement](e) with EcologyMember {
  
  lazy val PageManager = interface[querki.display.PageManager]
  
  def doRender() = 
    inp(cls:="search-query form-control _searchInput", 
      tpe:="text",
      placeholder:="Search")
  
  def hook() = {
    // For now, we're just going to deal with it when the user hits return.
    // TODO: in the long run, can we do prompting, a la Google?
    $(elem).keydown { (evt:JQueryEventObject) =>
      if (evt.which == 13) {
        val query = $(elem).value.asInstanceOf[String]
        PageManager.showPage("_search", Map("query" -> query))
        false
      }
    }
  }
}

class SearchResultsPage(params:ParamMap)(implicit e:Ecology) extends Page(e, "search") with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  val query = params("query")
  val queryLen = query.length
  
  def boldfaceResult(result:SearchResult):Modifier = {
    val text = result.text
  
    // We iterate through this via recursion, mostly because it comes out
    // decently elegant:
    def boldfaceRec(currentPos:Int, positions:List[Int]):List[Modifier] = {
      positions match {
        case pos :: rest => {
          // Next position of the search team. The stuff before it is left alone...
          val prefix = text.substring(currentPos, pos)
          val boldEnd = pos + queryLen
          // ... the search term itself gets highlighted...
          val boldfaced = span(cls:="searchResultHighlight", text.substring(pos, boldEnd))
          // ... and we recurse down the list for the rest of it:
          prefix :: boldfaced :: boldfaceRec(boldEnd, rest)
        }
        case Nil => {
          List(text.substring(currentPos))
        }
      }
    }
  
    boldfaceRec(0, result.positions)
  }
  
  def showResult(result:SearchResult):Modifier = {
    MSeq(
      dt(b(a(href:=thingUrl(result.thing), raw(result.thing.displayName))), s" (${result.propName})"),
      dd(cls:="_searchResult", pre(code(boldfaceResult(result))))
    )
  }

  def pageContent = {
    for {
      resultsOpt <- Client[SearchFunctions].search(query).call()
      guts = 
        div(
          resultsOpt match {
            case Some(results) if (results.results.size > 0) => {
              MSeq(
                h4(
                  cls:="_searchResultHeader", 
                  msg("resultsHeader", 
                      ("numFound" -> results.results.size.toString),
                      ("query" -> query))),
                dl(
                  for { 
                    result <- results.results.sortBy(_.score).reverse 
                  }
                    yield showResult(result)
                )
              )
            }
            case _ => h4(cls:="_searchResultHeader", msg("noResultsHeader", ("query" -> query)))
          }
        )
    }
      yield PageContents(pageTitleWith("query" -> query), guts)
  }
}
