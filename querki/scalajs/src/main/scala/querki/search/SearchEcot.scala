package querki.search

import querki.globals._
import querki.pages.{Page, PageFactory, ParamMap}

class SearchEcot(e:Ecology) extends ClientEcot(e) {

  def implements = Set.empty

  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Pages = interface[querki.pages.Pages]
  
  override def postInit() = {
    Gadgets.registerSimpleGadget("._searchInput", { new SearchGadget })
    Pages.registerStandardFactory("_search", { (params) => new SearchResultsPage(params) })
  }
}
