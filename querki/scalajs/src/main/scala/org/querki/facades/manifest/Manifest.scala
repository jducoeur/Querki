package org.querki.facades.manifest

import scala.scalajs.js
import js.annotation.JSName
import js.JSConverters._
import js.{Dynamic, UndefOr}

import org.scalajs.dom
import org.querki.jquery._
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
  def manifest(config:ManifestOptions):JQuery = js.native
  
  /**
   * Give a command to Manifest. Don't use this directly, use the better-typed signatures in
   * ManifestCommands instead.
   */
  @JSName("manifest")
  def manifestInternal(cmd:String, params:js.Any*):Any = js.native
}

/**
 * This class (which is implicitly converted from ManifestFacade or JQuery) wraps Manifest's
 * commands in strongly-typed signatures, to make them easier to call reliably from Scala.
 */
class ManifestCommands(manifest:ManifestFacade) {
  /**
   * Add one or more items to the end of the list.
   */
  def manifestAdd(data:Map[String,String]) = manifest.manifestInternal("add", data.toJSDictionary)
  
  /**
   * Get the container element.
   * 
   * TBD: is the return signature correct?
   */
  def manifestContainer():dom.Element = manifest.manifestInternal("container").asInstanceOf[dom.Element]
  
  /**
   * Remove the elements, events, and functionality of this plugin and return the input to its original state.
   */
  def manifestDestroy() = manifest.manifestInternal("destroy")
  
  /**
   * Get the list element.
   * 
   * TBD: is the return signature correct?
   */
  def manifestList():dom.Element = manifest.manifestInternal("list").asInstanceOf[dom.Element]
  
  /**
   * Get the entire options object.
   */
  def manifestGetOptions():js.Dictionary[js.Any] = manifest.manifestInternal("option").asInstanceOf[js.Dictionary[js.Any]]
  /**
   * Get a specific option.
   */
  def manifestGetOption(name:String):Any = manifest.manifestInternal("option", name)
  /**
   * Set a specific option.
   * 
   * Note that manifestSetOptions() is more strongly-typed, and recommended instead.
   */
  def manifestSetOption(name:String, value:js.Any) = manifest.manifestInternal("option", name, value)
  /**
   * Set multiple options.
   */
  def manifestSetOptions(opts:ManifestOptions) = manifest.manifestInternal("option", opts)
  
  /**
   * Remove one or more list items, specifying either jQuery objects or a selector that matches list children.
   */
  def manifestRemove(selector:ElementDesc) = manifest.manifestInternal("remove", selector.asInstanceOf[js.Any])
  
  /**
   * Get an array of the current values.
   */
  def manifestValues():Seq[String] = manifest.manifestInternal("values").asInstanceOf[js.Array[String]].toSeq
}

trait ManifestOptions extends js.Object 
object ManifestOptions extends ManifestOptionBuilder(noOpts)
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
  def marcoPolo(v:Boolean) = jsOpt("marcoPolo", v)
  
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
  def separator(v:js.Array[Int]) = jsOpt("separator", v)
  
  /**
   * One or more initial values to add to the list.
   * 
   * Default: null
   */
  def values(v:String) = jsOpt("values", v)
  def values(v:js.Any) = jsOpt("values", v)
  
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
   *   ...
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
  def formatDisplay(v:js.Function1[js.Any, js.Any]) = jsOpt("formatDisplay", v)
  def formatDisplay(v:js.ThisFunction3[JQuery, js.Any, JQuery, UndefOr[JQuery], js.Any]) = jsOpt("formatDisplay", v)
  
  /**
   * Format the display of the remove link included with each item. The returned value is added to $remove with the class mf_remove:
   * 
   * <li class="mf_item">
   *   ...
   *  <a href="#" class="mf_remove" title="Remove">X</a>
   *   ...
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
   *   ...
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
  def formatValue(v:js.Function1[js.Object, String]) = jsOpt("formatValue", v)
  def formatValue(v:js.ThisFunction4[JQuery, js.Object, JQuery, JQuery, UndefOr[JQuery], String]) = jsOpt("formatValue", v)
}
