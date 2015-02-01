package org.querki.jquery

import scala.scalajs.js
import org.scalajs.dom.XMLHttpRequest

trait JQueryXHR extends XMLHttpRequest {
  def overrideMimeType(): js.Dynamic = ???
}
