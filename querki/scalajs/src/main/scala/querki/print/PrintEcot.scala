package querki.print

import scala.scalajs.js
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
  
  /**
   * If we're not printing the page itself, we need to be clever. We are using the
   * approach suggested in this SO page:
   * 
   *     http://stackoverflow.com/questions/23707251/how-do-i-print-different-page-using-javascript-jquery-ajax
   *     
   * Basically, we create a hidden iframe, stuff the pageView contents into it, and print *that*.
   */
  def printPrintView(thing:ThingInfo, printView:Wikitext) = {
    val printFrame:TypedGadget[dom.HTMLIFrameElement] = iframe(cls:="_printFrame", display:="none", visibility:="hidden")
    $(window.document.body).append(printFrame.render)
    val printDoc = printFrame.elem.contentDocument.asInstanceOf[dom.HTMLDocument]
    // The styles from the main page don't affect the contents of the iframe, so we need to copy them in.
    // TODO: the current approach, of having the styles simply shoved into ThingPage, is probably wrong. Think
    // about where styles *should* go, so we can get rid of this coupling:
    $("#_pageStyles").each({ (e:dom.Element) =>
      $(printDoc.head).append($(e).clone())
    }:js.ThisFunction0[dom.Element, Any])
    $(printDoc.body).append(new QText(printView, cls:="_printView").render)
    printFrame.elem.contentWindow.print()
    // TODO: in principle, we should hook afterprint and remove the iframe. Not a major concern, though.
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
