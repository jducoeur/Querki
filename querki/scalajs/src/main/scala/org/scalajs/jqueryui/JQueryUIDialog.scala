package org.scalajs.jqueryui

import scala.scalajs.js
import js.{Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
import org.scalajs.jquery._
import org.scalajs.ext._

/**
 * Represents the jQuery UI Dialog Widget. Requires that the corresponding JavaScript code be loaded.
 * 
 * For more details, see:
 * 
 *     http://api.jqueryui.com/dialog/
 */
trait JQueryUIDialogFacade extends js.Object {
  /**
   * Defines this jQuery object as a Dialog, with the given options, and enables the
   * other commands to work.
   */
  def dialog(options:DialogOptions):JQuery = ???
  
  /**
   * Given a command to this dialog.
   */
  def dialog(cmd:String):Any = ???
}

trait DialogOptions extends js.Object 

object DialogOptions extends JSOptionBuilder[DialogOptions] {
  /**
   * Which element the dialog should be appended to.
   * 
   * Default: "body"
   */
  val appendTo = jsOpt[String]("appendTo")
  
  /**
   * If set to true, the dialog will automatically open upon initialization.
   * If false, the dialog will stay hidden until the open() method is called
   * 
   * Default: true
   */
  val autoOpen = jsOpt[Boolean]("autoOpen")
  
  /**
   * Specifies which buttons should be displayed on the dialog. 
   * The context of the callback is the dialog element; if you need access to the button, 
   * it is available as the target of the event object.
   * 
   * Note: for now, we are only supporting the Object version of this parameter. In jQuery UI,
   * there is also an array-based version, which is more powerful but more complex to use. It
   * is not yet obvious how best to express this union type in Scala.js.
   * 
   * Note also that we are being very specific about the type, rather than just saying js.Object.
   * It is important that this be made up of js.Function0's, or jQuery will break obscurely deep down.
   */
  val buttons = jsOpt[js.Dictionary[js.Function0[Any]]]("buttons")
  
  /**
   * Specifies whether the dialog should close when it has focus and the user presses the escape (ESC) key.
   * 
   * Default: true
   */
  val closeOnEscape = jsOpt[Boolean]("closeOnEscape")
  
  /**
   * Specifies the text for the close button. Note that the close text is visibly hidden when using a standard theme.
   * 
   * Default: "close"
   */
  val closeText = jsOpt[String]("closeText")
  
  /**
   * The specified class name(s) will be added to the dialog, for additional theming.
   */
  val dialogClass = jsOpt[String]("dialogClass")
  
  /**
   * If set to true, the dialog will be draggable by the title bar. Requires the jQuery UI Draggable widget to be included.
   * 
   * Default: true
   */
  val draggable = jsOpt[Boolean]("draggable")
  
  /**
   * The height of the dialog.
   * 
   * Note: technically, this is Number | String, but the only valid String value is the default of "auto".
   */
  val height = jsOpt[Int]("height")
  
  // This one is hard, since it takes many possible types. Think about how to support it.
  //var hide:UndefOr[???]
  
  /**
   * The maximum height to which the dialog can be resized, in pixels.
   */
  val maxHeight = jsOpt[Int]("maxHeight")
  
  /**
   * The maximum width to which the dialog can be resized, in pixels.
   */
  val maxWidth = jsOpt[Int]("maxWidth")
  
  /**
   * The minimum height to which the dialog can be resized, in pixels.
   * 
   * Default: 150
   */
  val minHeight = jsOpt[Int]("minHeight")
  
  /**
   * The minimum width to which the dialog can be resized, in pixels.
   */
  val minWidth = jsOpt[Int]("minWidth")
  
  /**
   * If set to true, the dialog will have modal behavior; other items on the page 
   * will be disabled, i.e., cannot be interacted with. Modal dialogs create an overlay 
   * below the dialog but above other page elements.
   * 
   * Default: false
   */
  val modal = jsOpt[Boolean]("modal")
  
  /**
   * Specifies where the dialog should be displayed when opened. The dialog will handle collisions
   * such that as much of the dialog is visible as possible.
   * 
   * The of property defaults to the window, but you can specify another element to position against. 
   * You can refer to the jQuery UI Position utility for more details about the available properties.
   * 
   * Default: { my: "center", at: "center", of: window }
   */
  val position = jsOpt[js.Object]("position")
  
  /**
   * If set to true, the dialog will be resizable. Requires the jQuery UI Resizable widget to be included.
   * 
   * Default: true
   */
  val resizable = jsOpt[Boolean]("resizable")
  
  // Like hide, this one is hard to define properly.
  //var show:UndefOr[???]
  
  /**
   * Specifies the title of the dialog. If the value is null, the title attribute on the dialog source element will be used.
   */
  val title = jsOpt[String]("title")
  
  /**
   * The width of the dialog, in pixels.
   * 
   * Default: 300
   */
  val width = jsOpt[Int]("width")
}
