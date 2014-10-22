package querki.pages

import querki.globals._

class PagesEcot(e:Ecology) extends ClientEcot(e) with Pages {
  
  def implements = Set(classOf[Pages])
  
  /**
   * The big hardcoded factory for Pages.
   * 
   * This is a bit inelegant and coupled, but there is no great answer, and it doesn't
   * actually violate DRY.
   */
  def constructPage(name:String, params:ParamMap):Page = {
    name match {
      case "_search" => new SearchResultsPage(params)
      case "_explore" => new ExplorePage(params)
      case _ => new ThingPage(name, params)
    }
  }
  
}
