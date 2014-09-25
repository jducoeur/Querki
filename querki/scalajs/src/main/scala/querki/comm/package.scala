package querki

import scala.scalajs.js

package object comm {
  
  type Call = String
  val emptyCall = ""
  
  val controllers = ClientRoutes.controllers
  def url(c:js.Dynamic):Call = c.url.toString

}
