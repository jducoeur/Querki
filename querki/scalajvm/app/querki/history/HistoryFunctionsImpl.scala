package querki.history

import querki.api._
import querki.globals._

class HistoryFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with HistoryFunctions {
  import HistoryFunctions._
  import SpaceHistory._
  
  def doRoute(req:Request):Future[String] = route[HistoryFunctions](this)(req)

  def getHistorySummary():Future[HistorySummary] = {
    // In theory, non-owner shouldn't be able to call this, but belt and suspenders:
    if (!isOwner)
      throw new Exception(s"Only the owner of the Space is allowed to view its history!")
    
    for {
      summary <- spaceRouter.requestFor[HistorySummary](GetHistorySummary())
    }
      yield summary
  }
}
