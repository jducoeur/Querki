package querki.pages

import scala.concurrent.Future

import org.scalajs.dom
import org.scalajs.jquery.JQueryEventObject
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import querki.globals._

import querki.api.SearchFunctions
import SearchFunctions._
import querki.display.Gadget

/**
 * The Gadget in the MenuBar, which initiates a Search. This mostly just leads to the
 * SearchResultsPage.
 */
class SearchGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLInputElement] with EcologyMember {
  
  lazy val PageManager = interface[querki.display.PageManager]
  
  def doRender() = 
    inp(cls:="search-query", 
      tpe:="text",
      placeholder:="Search")
  
  override def onCreate(elem:dom.HTMLInputElement) = {
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

class SearchResultsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
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
      dt(b(a(href:=thingUrl(result.thing), s"${result.thing.displayName} (${result.propName})"))),
      dd(pre(code(boldfaceResult(result))))
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
                h4(s"""Found ${results.results.size} matches for "$query""""),
                dl(
                  for { 
                    result <- results.results.sortBy(_.score).reverse 
                  }
                    yield showResult(result)
                )
              )
            }
            case _ => h4(s"""Nothing found for "$query"""")
          }
        )
    }
      yield PageContents(s"Search results for $query", guts)
  }
}
