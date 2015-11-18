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
   * It is recommended that the UI reload when this responds, since things may have changed a lot.
   * 
   * This API is highly experimental, and very likely to change.
   */
  def addApp(appId:String):Future[Unit]
  
  /**
   * Lift the specified elements from this Space into a newly-created App.
   * 
   * TODO: this should return a status stream handle, so we can monitor the progress of the extraction!
   */
  def extractApp(elements:Seq[TID]):Future[Unit]
}
