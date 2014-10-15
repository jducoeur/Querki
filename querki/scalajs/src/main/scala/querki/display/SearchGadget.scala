package querki.display

import scala.async.Async._

import autowire._

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all.{input => inp, _}

import querki.globals._

import querki.api.SearchFunctions
import SearchFunctions._

class SearchGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLInputElement] with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[StatusLine]
  
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
        println(s"Searching for $query")
        async {
          val resultsOpt = await(Client[SearchFunctions].search(query).call())
          resultsOpt match {
            case Some(results) if (results.results.size > 0) => {
              StatusLine.showBriefly(s"Found ${results.results.size} matches")
            }
            case _ => StatusLine.showBriefly(s"Nothing found for $query")
          }
        }
        false
      }
    }
  }
}
