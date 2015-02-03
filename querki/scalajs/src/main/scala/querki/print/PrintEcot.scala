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
    // This part is tricky: we want to copy in all of the stylesheets, but then we have to wait until they
    // finish loading before we actually print.
    // IMPORTANT: note the explicit assumption that the number of links is non-zero!
    var outstandingLinks = 0
    $("link").filter({ (index:Int, e:dom.Element) =>
      $(e).attr("rel") == "stylesheet"
    }).each({ (e:dom.Element) => 
      val newLink = $(e).clone()
      outstandingLinks += 1
      $(printDoc.head).append(newLink)
      $(newLink).on("load", null, null, { e:JQueryEventObject => 
        outstandingLinks -= 1
        if (outstandingLinks == 0) {
          // Okay, all of the links have loaded; we can now actually do the printing:
          printFrame.elem.contentWindow.print()
        }
        1:js.Any 
      })
    }:js.ThisFunction0[dom.Element, Any])
    $(printDoc.body).append(new QText(printView, cls:="_printView").render)
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
