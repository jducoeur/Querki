package querki

import scala.scalajs.js

import querki.globals._

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
        
  case class SpaceCall(call:js.Dynamic) {
    /**
     * Fetch this PlayCall using the current Space, plus whatever other parameters are needed.
     * 
     * This takes advantages of the fact that we've been consistent in our function calls: the
     * first two parameters are almost always the userName and spaceId. That being the case, those
     * can be auto-injected like this.
     */
    def spaceCall(params:js.Any*)(implicit ecology:Ecology):PlayCall = {
      val DataAccess = ecology.api[querki.data.DataAccess]
      call.apply((Seq[js.Any](DataAccess.userName, DataAccess.spaceId) ++ params):_*)
    }
    
    /**
     * Similar to spaceCall, but returns the URL of the PlayCall.
     */
    def spaceUrl(params:js.Any*)(implicit ecology:Ecology):URL = {
      spaceCall(params:_*).url
    }
  }
  
  /**
   * Enrich a named Play entry point with the spaceCall() and spaceUrl() methods.
   */
  implicit def dyn2SpaceCall(call:js.Dynamic) = SpaceCall(call)
}
