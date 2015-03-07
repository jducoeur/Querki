package querki.display

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.dom
import org.scalajs.jquery._
import org.querki.facades.jqueryui._

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
  height:Int, width:Int,
  guts:scalatags.JsDom.TypedTag[_],
  buttonsIn:(String, Dialog => Unit)*
  ) extends Gadget[dom.HTMLDivElement] 
{
  def doRender() = div(title:=dialogTitle, guts)
  
  def show() = {
    render
    // We want to pass the dialog into callbacks; this gets around some recursive-definition
    // difficulties that you can otherwise have.
    val buttons = buttonsIn.map { pair =>
      val (buttonName, cb) = pair
      (buttonName -> ({ () => cb(this) } : js.Function0[Any]))
    }
    val buttonMap = Map(buttons:_*).toJSDictionary
    val asDialog = $(elem).dialog(DialogOptions.
      title(dialogTitle).
      height(height).width(width).
      buttons(buttonMap)
    )
  }
  
  /**
   * Finish with this dialog.
   * 
   * IMPORTANT: this removes the underlying DOM elements! This Gadget should be considered dead
   * after this call!
   */
  def done() = {
    $(elem).dialog("destroy")
    $(elem).remove
  }
}
