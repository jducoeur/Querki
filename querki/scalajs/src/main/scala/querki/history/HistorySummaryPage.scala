package querki.history

import scalatags.JsDom.all._
import autowire._

import querki.globals._
import querki.pages._

import HistoryFunctions._

class HistorySummaryPage(params:ParamMap)(implicit e:Ecology) extends Page(e, "historySummary") with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  
  def pageContent = for {
    summary <- Client[HistoryFunctions].getHistorySummary().call()
    HistorySummary(evts, EvtContext(whoMap, thingNames)) = summary
    guts = div("hello")
  }
    yield PageContents(pageTitle, guts)
}
