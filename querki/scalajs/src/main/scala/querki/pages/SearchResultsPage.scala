package querki.pages

import scala.concurrent.Future

import scalatags.JsDom.all._

import querki.globals._

class SearchResultsPage(e:Ecology) extends Page(e) with EcologyMember {
  
//  def title = details.results.map(request => s"Search Results for $request").getOrElse("No results found")

  def pageContent = Future.successful(PageContents("TO DO", 
    div(
      p("Search results will go here")
    )))
}
