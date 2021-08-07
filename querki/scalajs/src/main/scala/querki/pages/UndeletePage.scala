package querki.pages

import scala.concurrent.Future

import scalatags.JsDom.all._
import autowire._

import querki.globals._
import querki.history.HistoryFunctions

/**
 * This intermediate page exists solely to undelete the specified Thing, and then go to it. It
 * gets invoked from a couple of places in the UI.
 */
class UndeletePage(params: ParamMap)(implicit val ecology: Ecology) extends Page("undelete") {

  lazy val Client = interface[querki.client.Client]

  lazy val thingId = params("thingId")
  lazy val thingName = params("thingName")

  def runUndelete() = {
    Client[HistoryFunctions].restoreDeletedThing(TID(thingId)).call().map { thingInfo =>
      Pages.thingPageFactory.showPage(thingInfo)
    }
  }

  def pageContent = {
    runUndelete()
    Future.successful(PageContents(msg("pageTitle", ("thingName" -> thingName)), div("Undeleting...")))
  }
}
