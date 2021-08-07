package querki.search

import querki.globals._
import querki.pages.{Page, PageFactory, ParamMap}

class SearchEcot(e: Ecology) extends ClientEcot(e) with Search {

  def implements = Set(classOf[Search])

  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Pages = interface[querki.pages.Pages]

  lazy val searchResultsFactory =
    Pages.registerStandardFactory("_searchResultsPage", { (params) => new SearchResultsPage(params) })

  override def postInit() = {
    Gadgets.registerSimpleGadget("._searchInput", { new SearchGadget })
    searchResultsFactory
  }
}
