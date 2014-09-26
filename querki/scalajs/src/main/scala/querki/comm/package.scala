package querki

import scala.scalajs.js

package object comm {
  
  type URL = String
  
  val controllers = ClientRoutes.controllers

  /**
   * Yes, this is slightly horrible, but I don't think there's a safer way to do it.
   */
  implicit def dynamic2PlayCall(dyn:js.Dynamic):PlayCall = dyn.asInstanceOf[PlayCall]

  /**
   * Enrich a PlayCall with the callAjax() method.
   */
  implicit def dynamic2Ajax(dyn:js.Dynamic):PlayAjax = new PlayAjax(dynamic2PlayCall(dyn))
  
  val EmptyCall:PlayCall = js.Dynamic.literal("url" -> "#")
}
