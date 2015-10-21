package querki.apps

import scala.concurrent.Future

import querki.data._

/**
 * API for functions specifically relating to App management.
 * 
 * @author jducoeur
 */
trait AppsFunctions {
  /**
   * Fetch the *immediate* parents of this Space.
   * 
   * This does not work recursively, at least yet. (That may change.)
   */
  def getApps():Seq[SpaceInfo]
  
  /**
   * Add the specified App to this Space, as a mix-in.
   * 
   * Responds with the info about the new App.
   * 
   * This API is highly experimental, and very likely to change.
   */
  def addApp(appId:String):Future[SpaceInfo]
}
