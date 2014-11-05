package querki.display

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.dom
import org.scalajs.jquery._
import org.scalajs.jqueryui._

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
  )(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember 
{
  def doRender() = div(title:=dialogTitle, guts)
  
  def show() = {
    render
    // We want to pass the dialog into callbacks; this gets around some recursive-definition
    // difficulties that you can otherwise have
    val buttons = buttonsIn.map { pair =>
      val (buttonName, cb) = pair
      (buttonName -> ({ () => cb(this) } : js.Function0[Any]))
    }
    val buttonMap = Map(buttons:_*).toJSDictionary
    
    val strs = buttonMap.keys.map { key => s"  $key = ${buttonMap(key)}" }
    println(s"The buttons are:\n${strs.mkString("\n")}")
    
    // Is this needed?
    $(elem).appendTo($("body"))
    
    val asDialog = $(elem).dialog(DialogOptions(
      title = dialogTitle,
      height = height, width = width,
//      buttons = lit(Delete = {() => println("You clicked the Delete button!") })
      buttons = buttonMap
    ))
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
