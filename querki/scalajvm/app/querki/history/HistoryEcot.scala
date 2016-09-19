package querki.history

import querki.ecology._
import querki.globals._
import querki.values.RequestContext

import HistoryFunctions._

class HistoryEcot(e:Ecology) extends QuerkiEcot(e) with History {
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[HistoryFunctions, HistoryFunctionsImpl](SpaceOps.spaceRegion, allowedDuringHistory = true)
  }
  
  def viewingHistoryVersion(rc:RequestContext):Option[HistoryVersion] = {
    rc.rawParam("_historyVersion") match {
      case Some(vStr) => {
        Some(vStr.toLong)
      }
      case _ => None
    }
  }
  
  def isViewingHistory(rc:RequestContext):Boolean = {
    rc.rawParam("_historyVersion").map(_.length > 0).getOrElse(false)
  }
}
