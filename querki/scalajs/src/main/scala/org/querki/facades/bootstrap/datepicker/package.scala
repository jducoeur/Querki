package org.querki.facades.bootstrap

import scala.scalajs.js
import scala.scalajs.js._
import org.querki.jquery.JQuery

/**
 * This package provides a facade for Bootstrap Datepicker, a third-party plugin
 * for Bootstrap that can be found at:
 * 
 *   https://github.com/eternicode/bootstrap-datepicker
 *   
 * As the name implies, this provides a cross-platform, responsive UI for choosing a
 * date. In principle, it would be ideal to do this as a polyfill instead, based on
 * the HTML5 date input type; in practice, date isn't well-supported, and I haven't
 * loved any of the polyfills I've encountered yet. So for now, this UI still seems
 * useful.
 */
package object datepicker {
  implicit def jq2Datepicker(jq:JQuery):BootstrapDatepicker = jq.asInstanceOf[BootstrapDatepicker]
  implicit def jq2DatepickerCommands(jq:JQuery): DatepickerCommands = new DatepickerCommands(jq)
  
  type BeforeShowFunc = js.Function1[Date, UndefOr[Boolean | String | js.Dictionary[Any]]]
}