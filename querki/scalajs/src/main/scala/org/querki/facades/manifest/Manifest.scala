package org.querki.facades.manifest

import scala.scalajs.js
import org.scalajs.jquery.JQuery
import js.{Dynamic, UndefOr}
import org.querki.jsext._

/**
 * Facade for Manifest, an easy-to-use package for prompting tags.
 * 
 * See:
 *     https://github.com/jstayton/jquery-manifest
 *     
 * Note that Manifest depends on MarcoPolo.
 */
trait ManifestFacade extends js.Object {
  def manifest(config:ManifestOptions):JQuery = ???
}

trait ManifestOptions extends js.Object 

class ManifestOptionBuilder(val dict:OptMap) extends JSOptionBuilder[ManifestOptions, ManifestOptionBuilder](new ManifestOptionBuilder(_)) {
  
  /////////////////////////////////////////
  //
  // Options
  //
  
  /**
   * Options to pass on to Marco Polo for autocomplete functionality. Set to false if such functionality is unnecessary.
   * 
   * Default: false
   */
  def marcoPolo(v:MarcoPoloOptions) = jsOpt("marcoPolo", v)
  
  /**
   * Whether to only allow items to be selected from the autocomplete results list when autocomplete is enabled. 
   * If false, arbitrary, non-results-list values can be added when the separator key character is pressed or the input is blurred.
   * 
   * Default: false
   */
  def required(v:Boolean) = jsOpt("required", v)
  
  /**
   * One or more key characters or codes to separate arbitrary, non-results-list values if the required option is false. 
   * Pressing one of these keys will add the current input value to the list. Also used to split the initial input 
   * value and pasted values.
   * 
   * Default: ,
   */
  def separator(v:String) = jsOpt("separator", v)
  def separator(v:js.Array[String]) = jsOpt("separator", v)
  
  /**
   * One or more initial values to add to the list.
   * 
   * Default: null
   */
  def values(v:String) = jsOpt("values", v)
  def values(v:js.Object) = jsOpt("values", v)
  
  /**
   * Name of the hidden input value fields. Do not include [] at the end, as that will be added. If unset, 
   * the default is to add __values[]_ to the input name.
   * 
   * Default: null
   */
  def valuesName(v:String) = jsOpt("valuesName", v)
  
  
  ///////////////////////////////////////////
  //
  // Callbacks
  //
  
  /**
   * Format the display of an item. The returned value is added to $item with the class mf_item:
   * 
   * <li class="mf_item">
   *   "Lindsay Weir" (lweir65@gmail.com)
   *   …
   * </li>
   * 
   * Default:
   * 
   * if ($mpItem) {
   *   return $mpItem.html();
   * }
   * else {
   *   return data;
   * }
   * 
   * Parameters:
   * 
   * data string, object Item data.
   * $item jQuery object List item that will be used for display.
   * $mpItem jQuery object, null Optional Marco Polo selected list item.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: string, DOM element, or jQuery object to use as the display. A Deferred object can also be 
   * returned if an asynchronous process needs to be run that resolves with one of these types later.
   */
  def formatDisplay(v:js.ThisFunction3[JQuery, js.Any, JQuery, UndefOr[JQuery], js.Any]) = jsOpt("formatDisplay", v)
  
  /**
   * Format the display of the remove link included with each item. The returned value is added to $remove with the class mf_remove:
   * 
   * <li class="mf_item">
   *   …
   *  <a href="#" class="mf_remove" title="Remove">X</a>
   *   …
   * </li>
   * 
   * Default:
   * 
   * return 'X';
   * 
   * Parameters:
   *
   * $remove jQuery object Remove link.
   * $item jQuery object List item that will be added.
   * this: jQuery object Text input (no need to wrap like $(this)).
   * 
   * Return: string, DOM element, or jQuery object to use as the display. A Deferred object can also be returned 
   * if an asynchronous process needs to be run that resolves with one of these types later.
   */
  def formatRemove(v:js.ThisFunction2[JQuery, JQuery, JQuery, js.Any]) = jsOpt("formatRemove", v)
  
  /**
   * Format the hidden value to be submitted for the item. The returned value is set as the value of $value with the class mf_value:
   * 
   * <li class="mf_item">
   *   …
   *   <input type="hidden" class="mf_value" value="lweir65@gmail.com">
   * </li>
   * 
   * Default:
   * 
   * if ($mpItem) {
   *   return $mpItem.text();
   * }
   * else {
   *   return data;
   * }
   * 
   * Parameters:
   *
   * data string, object Item data.
   * $value jQuery object Hidden value element that will be added.
   * $item jQuery object List item that will be added.
   * $mpItem jQuery object, null Optional Marco Polo selected list item.
   * this: jQuery object Text input (no need to wrap like $(this)).
   *
   * Return: string value. A Deferred object can also be returned if an asynchronous process needs to be run that resolves with a string value later.
   */
  def formatValue(v:js.ThisFunction4[JQuery, js.Object, JQuery, JQuery, UndefOr[JQuery], String]) = jsOpt("formatValue", v)
}
