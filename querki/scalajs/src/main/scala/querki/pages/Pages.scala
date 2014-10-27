package querki.pages

import querki.globals._

class PagesEcot(e:Ecology) extends ClientEcot(e) with Pages {
  
  def implements = Set(classOf[Pages])
  
  /**
   * The big hardcoded factory for Pages.
   * 
   * This is a bit inelegant and coupled, but there is no great answer, and it doesn't
   * actually violate DRY.
   * 
   * TODO: a better way for this to work would be for each Page to have a factory, and
   * have its corresponding Ecot register that factory in postInit(). That probably implies
   * moving each Page to the relevant package, but that seems more and more correct anyway.
   */
  def constructPage(name:String, params:ParamMap):Page = {
    name match {
      case "_search" => new SearchResultsPage(params)
      case "_explore" => new ExplorePage(params)
      case "_notifications" => new querki.notifications.NotificationsPage(params)
      case _ => new ThingPage(name, params)
    }
  }
  
}
