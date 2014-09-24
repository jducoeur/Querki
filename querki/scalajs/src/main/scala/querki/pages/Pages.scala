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
  def constructPage(id:PageIDs.PageID, pickled:String):Page = {
    id match {
      case PageIDs.ThingPage => new ThingPage(ecology, pickled)
    }
  }
  
}