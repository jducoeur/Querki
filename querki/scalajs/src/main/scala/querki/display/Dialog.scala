package querki.display

import scala.scalajs.js
import js.JSConverters._
import org.querki.jquery._
import org.scalajs.dom.{raw => dom}
import org.querki.facades.bootstrap._

import scalatags.JsDom.all._

import querki.globals._

/**
 * The abstraction of a popup dialog.
 * 
 * For the moment, this assumes that dialogs are one-offs, and are not reused. It will need
 * enhancement if that changes.
 * 
 * At the moment, this is implemented on top of jQuery UI, but that is intentionally hidden.
 * Try to keep the API clean and Scala-oriented. 
 */
class Dialog(
  dialogTitle:String,
  guts:scalatags.JsDom.TypedTag[_],
  buttonsIn:(String, String, Dialog => Unit)*
  )(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] 
{
  def doRender() = 
    div(
      cls := "modal fade",
      tabindex := -1,
      role := "dialog",
      aria.labelledby := dialogTitle,
      div(
        cls := "modal-dialog _querkiDialog", role := "document",
        div(
          cls := "modal-content",
          
          // The header:
          div(
            cls := "modal-header",
            button(tpe := "button", cls := "close", data("dismiss") := "modal", aria.label := "Close",
              span(aria.hidden := true, raw("&times;"))
            ),
            h4(cls := "modal-title", dialogTitle)
          ),
          
          // The content, as provided:
          div(
            cls := "_guts",
            guts
          ),
          
          // The footer, with the buttons:
          div(
            cls := "modal-footer",
            buttonsIn.map { btnInfo =>
              val (buttonName, idStr, cb) = btnInfo
              new ButtonGadget(ButtonGadget.Normal, id := idStr, buttonName)({() => cb(this) })
            }
          )
        )
      )
    )
    
  def show() = {
    render
    // TODO: this likely requires a hack to work correctly with iOS. See:
    //   http://www.abeautifulsite.net/bootstrap-3-modals-and-the-ios-virtual-keyboard/
    // The issue is that, if the guts contain input fields, so it brings up the virtual keyboard,
    // it doesn't move the dialog. So it might get obscured by the keyboard.
    $(elem).modal(ModalCommand.show)
  }
  
  def done() = $(elem).modal(ModalCommand.hide)
}
