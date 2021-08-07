package querki.history

import autowire.Core.Request

import querki.globals._
import querki.history.HistoryFunctions._
import querki.pages.{Page, PageFactory, ParamMap}
import querki.time.displayTime
import querki.time.Common.Timestamp

class HistoryEcot(e: Ecology) extends ClientEcot(e) with History {
  def implements = Set(classOf[History])

  lazy val Pages = interface[querki.pages.Pages]

  lazy val historySummaryFactory =
    Pages.registerStandardFactory("_historySummary", { (params) => new HistorySummaryPage(params) })

  /**
   * If we are in View History mode, which version are we viewing?
   */
  var currentHistoryVersion: Option[HistoryVersion] = None
  var _currentHistoryTime: Option[Timestamp] = None

  def currentHistoryTime: Option[String] = {
    _currentHistoryTime.map(displayTime(_))
  }

  def setHistoryVersion(
    v: HistoryVersion,
    time: Timestamp
  ) = {
    currentHistoryVersion = Some(v)
    _currentHistoryTime = Some(time)
  }

  def clearHistoryVersion() = {
    currentHistoryVersion = None
    _currentHistoryTime = None
  }
  def viewingHistory = currentHistoryVersion.isDefined

  override def postInit() = {
    historySummaryFactory
  }

  def isLegalDuringHistory(req: Request[String]): Boolean = {
    // TODO: this implementation is grotesque. Can we come up with something better that lets us
    // set attributes or market traits on the API traits? Ideally, we'd like to be able to mark
    // just the APIs, and have both client and server draw from that.
    req.path match {
      // Allow *nearly* the entire ThingFunctions API
      case "querki" :: "api" :: "ThingFunctions" :: "deleteThing" :: rest => false
      case "querki" :: "api" :: "ThingFunctions" :: rest                  => true
      case "querki" :: "history" :: "HistoryFunctions" :: rest            => true
      case "querki" :: "search" :: "SearchFunctions" :: rest              => true
      case _                                                              => false
    }
  }
}
