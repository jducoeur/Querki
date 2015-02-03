package querki.print

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import models.Wikitext
import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.{QText, TypedGadget}

class PrintEcot(e:Ecology) extends ClientEcot(e) with Print {
  
  def implements = Set(classOf[Print])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[querki.display.PageManager]
  
  def window = PageManager.window
  
  def printPrintView(thing:ThingInfo, printView:Wikitext) = {
    val printFrame:TypedGadget[dom.HTMLIFrameElement] = iframe(display:="none", visibility:="hidden")
    $(window.document.body).append(printFrame.render)
    $(printFrame.elem.contentDocument.asInstanceOf[dom.HTMLDocument].body).append(new QText(printView).render)
    printFrame.elem.contentWindow.print()
  }

  def print(thing:ThingInfo) = {
    for {
      std <- DataAccess.standardThings
      printViewOpt <- Client[ThingFunctions].getPropertyDisplay(thing.oid, std.basic.printView.oid).call()
    }
      printViewOpt match {
        // There is a Print View on this Thing, so use that:
        case Some(printView) => printPrintView(thing, printView)
        // No Print View, so just print the page directly:
        case None => window.print()
      }
  }
}
