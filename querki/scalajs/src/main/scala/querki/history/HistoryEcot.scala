package querki.history

import querki.globals._
import querki.pages.{Page, PageFactory, ParamMap}

class HistoryEcot(e:Ecology) extends ClientEcot(e) with History {
  def implements = Set(classOf[History])
  
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val historySummaryFactory = Pages.registerStandardFactory("_historySummary", { (params) => new HistorySummaryPage(params) })
  
  override def postInit() = {
    historySummaryFactory
  }
}
