package querki.pages

import scalatags.JsDom.all._

import querki.globals._

class SearchResultsPage(e:Ecology) extends Page(e) with EcologyMember {
  
  type DetailsType = SearchPageDetails
  
  def title = "TO DO"
  
//  def title = details.results.map(request => s"Search Results for $request").getOrElse("No results found")

  def pageContent = 
    div(
      p("Search results will go here")
    )
}
