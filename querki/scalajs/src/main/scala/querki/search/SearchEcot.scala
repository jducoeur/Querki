package querki.search

import querki.globals._
import querki.pages.{Page, PageFactory, ParamMap}

class SearchEcot(e:Ecology) extends ClientEcot(e) with PageFactory {

  def implements = Set.empty
  
  lazy val Pages = interface[querki.pages.Pages]
  
  override def postInit() = {
    Pages.registerFactory(this)
  }
  
  def constructPageOpt(pageName:String, params:ParamMap):Option[Page] = {
    if (pageName == "_search")
      Some(new SearchResultsPage(params))
    else
      None
  }
}
