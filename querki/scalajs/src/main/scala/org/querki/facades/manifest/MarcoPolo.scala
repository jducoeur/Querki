package org.querki.facades.manifest

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import js.{Dynamic, UndefOr}
import org.querki.jsext._

/**
 * Facade for MarcoPolo, a package for prompting input through communication with the server.
 *
 * See:
 *     https://github.com/jstayton/jquery-marcopolo
 *
 * NOTE: the options for this have mostly not been tested yet!
 *
 * TODO: add Events and Methods
 */
@js.native
trait MarcoPoloFacade extends js.Object {
  def marcoPolo(config: MarcoPoloOptions): JQuery = js.native
}

@js.native
trait MarcoPoloOptions extends js.Object
object MarcoPoloOptions extends MarcoPoloOptionBuilder(noOpts)

class MarcoPoloOptionBuilder(val dict: OptMap)
  extends JSOptionBuilder[MarcoPoloOptions, MarcoPoloOptionBuilder](new MarcoPoloOptionBuilder(_)) {

  ///////////////////////////////////
  //
  // Options
  //

  /**
   * Whether to cache query results. The cache is shared by all instances, which is a big advantage when many
   * of the same field type appear on the same page. For example, a tags field that's repeated for every record on a page.
   *
   * Default: true
   */
  def cache(v: Boolean) = jsOpt("cache", v)

  /**
   * Whether to compare the selected item against items displayed in the results list. The selected item is
   * highlighted if a match is found, instead of the first item in the list (highlight option must be enabled).
   * Set this option to true if the data is a string; otherwise, specify the data object attribute name to compare on.
   *
   * Default: false
   */
  def compare(v: Boolean) = jsOpt("compare", v)
  def compare(v: String) = jsOpt("compare", v)

  /**
   * Additional data to be sent in the request query string. (Note: The query string parameter that is set
   * with the input value (param option) will overwrite the value in the data object if an attribute with the same name exists.)
   *
   * Default: {}
   *
   * When a function is used, it's called for every request, allowing the data to be dynamic. An object must be returned.
   *
   * Parameters:
   *
   * q string Requested input value.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: object of additional data.
   */
  def data(v: String) = jsOpt("data", v)
  def data(v: js.Object) = jsOpt("data", v)
  def data(v: js.ThisFunction1[JQuery, String, js.Object]) = jsOpt("data", v)

  /**
   * The number of milliseconds to delay before firing a request after a change is made to the input value.
   * This helps prevent an ajax request on every keystroke from overwhelming the server and slowing down the page.
   *
   * Default: 250
   */
  def delay(v: Int) = jsOpt("delay", v)

  /**
   * Whether to hide the results list when an item is selected. Interesting things can be done when this is set to
   * false, such as hiding and showing certain items when other items are selected. The results list is still
   * hidden when the input is blurred for any other reason.
   *
   * Default: true
   */
  def hideOnSelect(v: Boolean) = jsOpt("hideOnSelect", v)

  /**
   * Whether to automatically highlight an item when the results list is displayed. Usually it's the first item,
   * but it could be the previously selected item if compare is specified.
   *
   * Default: true
   */
  def highlight(v: Boolean) = jsOpt("highlight", v)

  /**
   * Positioning a label over an input is a common design pattern (sometimes referred to as overlabel) that
   * unfortunately doesn't work so well with all of the input focus/blur events that occur with autocomplete.
   * With this option, however, the hiding/showing of the label is handled internally to provide a built-in
   * solution to the problem. The label receives the class mp_label.
   *
   * Default: null
   *
   * TODO: this is actually Selector, not String.
   */
  def label(v: String) = jsOpt("label", v)
  def label(v: JQuery) = jsOpt("label", v)
  def label(v: dom.Element) = jsOpt("label", v)

  /**
   * The minimum number of characters required before a request is fired. See the formatMinChars callback
   * to format the (optional) message displayed when this value is not reached.
   *
   * Default: 1
   */
  def minChars(v: Int) = jsOpt("minChars", v)

  /**
   * The name of the query string parameter that is set with the input value.
   *
   * Default: q
   */
  def param(v: String) = jsOpt("param", v)

  /**
   * Whether to clear the input value when no selection is made from the results list. This happens when
   * the input is blurred, usually by clicking or tabbing out of the field.
   *
   * Default: false
   */
  def required(v: Boolean) = jsOpt("required", v)

  /**
   * The list items to make selectable. For example, say you add the class header to a number of list items
   * (in the formatItem callback) that you want to act as non-selectable headers. They can be excluded with
   * the selector :not(.header). Selectable items receive the class mp_selectable.
   *
   * Default: *
   */
  def selectable(v: String) = jsOpt("selectable", v)

  /**
   * Prime the input with a selected item. onSelect is called just as if the item were selected from the results list.
   *
   * Default: null
   */
  def selected(v: js.Object) = jsOpt("selected", v)

  /**
   * Whether to allow the browser's default behavior of submitting the form on ENTER.
   *
   * Default: false
   */
  def submitOnEnter(v: Boolean) = jsOpt("selectOnEnter", v)

  /**
   * The URL to GET request for the results, which must be an array of strings or JSON. If no URL is set,
   * the parent form's action attribute value is used if one exists. q is added to the query string with
   * the input value, along with any additional data.
   *
   * Default: null
   */
  def url(v: String) = jsOpt("url", v)

  ////////////////////////////////////
  //
  // Callbacks
  //
  // TODO: many of these have messy but finite return types, which should be tightened up.
  //

  /**
   * Format the raw data that's returned from the ajax request. Useful for further filtering the data or
   * returning the array of results that's embedded deeper in the object.
   *
   * Default: null
   *
   * Parameters:
   *
   * data array of object Data returned from the request.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: array of objects to use as the data.
   */
  def formatData(v: js.ThisFunction1[JQuery, js.Array[js.Object], js.Array[js.Object]]) = jsOpt("formatData", v)
  def formatData(v: js.Function1[js.Array[js.Object], js.Array[js.Object]]) = jsOpt("formatData", v)

  /**
   * Format the text that's displayed when the ajax request fails. The message is displayed in a list item with the class mp_error:
   *
   * <li class="mp_error">
   *   <em>Your search could not be completed at this time.</em>
   *   </li>
   * Setting this option to null or returning false suppresses the message from being displayed.
   *
   * Default:
   *
   * return '<em>Your search could not be completed at this time.</em>';
   *
   * Parameters:
   *
   * $item jQuery object List item to display the message.
   * jqXHR object or XMLHTTPRequest in jQuery 1.4.x.
   * textStatus string Error status of the request.
   * errorThrown string HTTP error status.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: string, DOM element, or jQuery object to use as the message.
   */
  def formatError(v: js.ThisFunction4[JQuery, JQuery, js.Dynamic, String, String, js.Any]) = jsOpt("formatError", v)

  /**
   * Format the display of each item in the results list. By default, the title or name value of the data object is
   * displayed. The returned value is added to a list item with the class mp_item:
   *
   * <li class="mp_item">The Title of Something</li>
   *
   * Default:
   *
   * return data.title || data.name;
   *
   * Parameters:
   *
   * data string or object Data returned from the request.
   * $item jQuery object List item to display the result.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: string, DOM element, or jQuery object to use as the display.
   */
  def formatItem(v: js.ThisFunction2[JQuery, js.Dynamic, JQuery, js.Any]) = jsOpt("formatItem", v)
  def formatItem(v: js.ThisFunction1[JQuery, js.Dynamic, js.Any]) = jsOpt("formatItem", v)
  def formatItem(v: js.Function1[js.Dynamic, js.Any]) = jsOpt("formatItem", v)

  /**
   * Format the text that's displayed when the minimum number of characters (specified with the minChars option)
   * hasn't been reached. The message is displayed in a list item with the class mp_min_chars:
   *
   * <li class="mp_min_chars">
   *   <em>Your search must be at least <strong>3</strong> characters.</em>
   * </li>
   *
   * Setting this option to null or returning false suppresses the message from being displayed. It is also not
   * displayed when there is no input value.
   *
   * Default:
   *
   * return '<em>Your search must be at least <strong>' + minChars + '</strong>characters.</em>';
   *
   * Parameters:
   *
   * minChars integer Minimum number of characters required.
   * $item jQuery object List item to display the message.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: string, DOM element, or jQuery object to use as the message.
   */
  def formatMinChars(v: js.ThisFunction2[JQuery, Int, JQuery, js.Any]) = jsOpt("formatMinChars", v)

  /**
   * Format the text that's displayed when there are no results returned for the requested input value. The
   * message is displayed in a list item with the class mp_no_results:
   *
   * <li class="mp_no_results">
   *   <em>No results for <strong>something</strong>.</em>
   * </li>
   *
   * Setting this option to null or returning false suppresses the message from being displayed.
   *
   * Default:
   *
   * return '<em>No results for <strong>' + q + '</strong>.</em>';
   *
   * Parameters:
   *
   * q string Requested input value.
   * $item jQuery object List item to display the message.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: string, DOM element, or jQuery object to use as the message.
   */
  def formatNoResults(v: js.Function1[String, js.Any]) = jsOpt("formatNoResults", v)
  def formatNoResults(v: js.ThisFunction2[JQuery, String, JQuery, js.Any]) = jsOpt("formatNoResults", v)

  ////////////////////////////////////
  //
  // Events
  //
  // TODO: finish filling these in
  //

  /**
   * Called when an item is selected from the results list or an initial value (see Setting an Initial Value).
   * By default, the title or name value of the data object is used to populate the input value.
   *
   * Default:
   *
   * this.val(data.title || data.name);
   *
   * Parameters:
   *
   * data string, object Data returned from the request.
   * $item jQuery object, null Selected results list item. null if selected option used.
   * initial boolean Whether this is an initial value.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Event: You can also bind to the marcopoloselect event:
   *
   * $(selector).on('marcopoloselect', function (event, data, $item, initial) { ... });
   */
  def onSelect(v: js.ThisFunction3[JQuery, js.Any, JQuery, Boolean, Any]) = jsOpt("onSelect", v)
  def onSelect(v: js.Function1[js.Any, Any]) = jsOpt("onSelect", v)
}
