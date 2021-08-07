package org.querki.facades.jqueryui

import scala.scalajs.js
import js.{|, Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
import org.querki.jquery._
import org.querki.jsext._

/**
 * Represents the jQuery UI Dialog Widget. Requires that the corresponding JavaScript code be loaded.
 *
 * For more details, see:
 *
 *     http://api.jqueryui.com/dialog/
 */
@js.native
trait JQueryUIDialogFacade extends js.Object {

  /**
   * Defines this jQuery object as a Dialog, with the given options, and enables the
   * other commands to work.
   */
  def dialog(options: DialogOptions): JQuery = js.native

  /**
   * Given a command to this dialog.
   *
   * TODO: see the ManifestFacade for a stronger way to do this:
   */
  def dialog(cmd: String): Any = js.native
}

@js.native
trait DialogOptions extends js.Object
object DialogOptions extends DialogOptionBuilder(noOpts)

class DialogOptionBuilder(val dict: OptMap)
  extends JSOptionBuilder[DialogOptions, DialogOptionBuilder](new DialogOptionBuilder(_)) {

  /**
   * Which element the dialog should be appended to.
   *
   * Default: "body"
   */
  def appendTo(v: String) = jsOpt("appendTo", v)

  /**
   * If set to true, the dialog will automatically open upon initialization.
   * If false, the dialog will stay hidden until the open() method is called
   *
   * Default: true
   */
  def autoOpen(v: Boolean) = jsOpt("autoOpen", v)

  /**
   * Specifies which buttons should be displayed on the dialog.
   * The context of the callback is the dialog element; if you need access to the button,
   * it is available as the target of the event object.
   *
   * Note that we are being very specific about the type, rather than just saying js.Object.
   * It is important that this be made up of js.Function0's, or jQuery will break obscurely deep down.
   *
   * @param v The keys are the button labels and the values are DialogButton structures.
   */
  def buttons(v: js.Dictionary[DialogButton]) = jsOpt("buttons", v)

  /**
   * Specifies which buttons should be displayed on the dialog.
   * The context of the callback is the dialog element; if you need access to the button,
   * it is available as the target of the event object.
   *
   * Note that we are being very specific about the type, rather than just saying js.Object.
   * It is important that this be made up of js.Function0's, or jQuery will break obscurely deep down.
   *
   * @param v Each element of the array must be an object defining the attributes, properties, and event handlers to set on the button.
   */
  def buttons(v: js.Array[DialogButton]) = jsOpt("buttons", v)

  /**
   * Specifies whether the dialog should close when it has focus and the user presses the escape (ESC) key.
   *
   * Default: true
   */
  def closeOnEscape(v: Boolean) = jsOpt("closeOnEscape", v)

  /**
   * Specifies the text for the close button. Note that the close text is visibly hidden when using a standard theme.
   *
   * Default: "close"
   */
  def closeText(v: String) = jsOpt("closeText", v)

  /**
   * The specified class name(s) will be added to the dialog, for additional theming.
   */
  def dialogClass(v: String) = jsOpt("dialogClass", v)

  /**
   * If set to true, the dialog will be draggable by the title bar. Requires the jQuery UI Draggable widget to be included.
   *
   * Default: true
   */
  def draggable(v: Boolean) = jsOpt("draggable", v)

  /**
   * The height of the dialog.
   *
   * Note: technically, this is Number | String, but the only valid String value is the default of "auto".
   */
  def height(v: Int) = jsOpt("height", v)

  /**
   * If and how to animate the hiding of the dialog.
   *
   * When set to false, no animation will be used and the dialog will be
   * hidden immediately. When set to true, the dialog will fade out with the default duration and the default easing.
   */
  def hide(v: Boolean) = jsOpt("hide", v)

  /**
   * If and how to animate the hiding of the dialog.
   *
   * The dialog will fade out with the specified duration and the default easing.
   */
  def hide(v: Int) = jsOpt("hide", v)

  /**
   * If and how to animate the hiding of the dialog.
   *
   * The dialog will be hidden using the specified effect. The value can either be the name
   * of a built-in jQuery animation method, such as "slideUp", or the name of a jQuery UI effect,
   * such as "fold". In either case the effect will be used with the default duration and the default easing.
   */
  def hide(v: String) = jsOpt("hide", v)

  /**
   * If and how to animate the hiding of the dialog.
   *
   * If the value is an object, then effect, delay, duration, and easing properties may
   * be provided. If the effect property contains the name of a jQuery method, then that method
   * will be used; otherwise it is assumed to be the name of a jQuery UI effect. When using a jQuery
   * UI effect that supports additional settings, you may include those settings in the object and they
   * will be passed to the effect. If duration or easing is omitted, then the default values will be used.
   * If effect is omitted, then "fadeOut" will be used. If delay is omitted, then no delay is used.
   */
  def hide(v: js.Object) = jsOpt("hide", v)

  /**
   * The maximum height to which the dialog can be resized, in pixels.
   */
  def maxHeight(v: Int) = jsOpt("maxHeight", v)

  /**
   * The maximum width to which the dialog can be resized, in pixels.
   */
  def maxWidth(v: Int) = jsOpt("maxWidth", v)

  /**
   * The minimum height to which the dialog can be resized, in pixels.
   *
   * Default: 150
   */
  def minHeight(v: Int) = jsOpt("minHeight", v)

  /**
   * The minimum width to which the dialog can be resized, in pixels.
   */
  def minWidth(v: Int) = jsOpt("minWidth", v)

  /**
   * If set to true, the dialog will have modal behavior; other items on the page
   * will be disabled, i.e., cannot be interacted with. Modal dialogs create an overlay
   * below the dialog but above other page elements.
   *
   * Default: false
   */
  def modal(v: Boolean) = jsOpt("modal", v)

  /**
   * Specifies where the dialog should be displayed when opened. The dialog will handle collisions
   * such that as much of the dialog is visible as possible.
   *
   * The of property defaults to the window, but you can specify another element to position against.
   * You can refer to the jQuery UI Position utility for more details about the available properties.
   *
   * TODO: this should take a proper strongly-typed structure!
   *
   * Default: { my: "center", at: "center", of: window }
   */
  def position(v: js.Object) = jsOpt("position", v)

  /**
   * If set to true, the dialog will be resizable. Requires the jQuery UI Resizable widget to be included.
   *
   * Default: true
   */
  def resizable(v: Boolean) = jsOpt("resizable", v)

  /**
   * If and how to animate the showing of the dialog.
   *
   * @param v When set to false, no animation will be used and the dialog will be
   *   shown immediately. When set to true, the dialog will fade in with the default duration and the default easing.
   */
  def show(v: Boolean) = jsOpt("show", v)

  /**
   * If and how to animate the showing of the dialog.
   *
   * @param v The dialog will fade in with the specified duration and the default easing.
   */
  def show(v: Int) = jsOpt("show", v)

  /**
   * If and how to animate the showing of the dialog.
   *
   * The dialog will be shown using the specified effect. The value can either be the name
   * of a built-in jQuery animation method, such as "slideDown", or the name of a jQuery UI effect,
   * such as "fold". In either case the effect will be used with the default duration and the default easing.
   */
  def show(v: String) = jsOpt("show", v)

  /**
   * If and how to animate the showing of the dialog.
   *
   * If the value is an object, then effect, delay, duration, and easing properties may
   * be provided. If the effect property contains the name of a jQuery method, then that method
   * will be used; otherwise it is assumed to be the name of a jQuery UI effect. When using a jQuery
   * UI effect that supports additional settings, you may include those settings in the object and they
   * will be passed to the effect. If duration or easing is omitted, then the default values will be used.
   * If effect is omitted, then "fadeIn" will be used. If delay is omitted, then no delay is used.
   */
  def show(v: js.Object) = jsOpt("show", v)

  /**
   * Specifies the title of the dialog. If the value is null, the title attribute on the dialog source element will be used.
   */
  def title(v: String) = jsOpt("title", v)

  /**
   * The width of the dialog, in pixels.
   *
   * Default: 300
   */
  def width(v: Int) = jsOpt("width", v)
}

/**
 * This represents the configuration for the buttons in a Dialog. Note that this is based on fairly poor
 * documentation; indeed, the fact that "id" works comes from StackOverflow.
 */
@js.native
trait DialogButton extends js.Object
object DialogButton extends DialogButtonBuilder(noOpts)

class DialogButtonBuilder(val dict: OptMap)
  extends JSOptionBuilder[DialogButton, DialogButtonBuilder](new DialogButtonBuilder(_)) {

  /**
   * The label to show on the button.
   */
  def text(v: String) = jsOpt("text", v)

  /**
   * The id for this button.
   */
  def id(v: String) = jsOpt("id", v)

  /**
   * The callback for when this button is clicked.
   */
  def click(v: js.Function0[Any]) = jsOpt("click", v)
}
