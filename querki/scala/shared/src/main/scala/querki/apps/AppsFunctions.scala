package querki.apps

import scala.concurrent.Future

import querki.data._

/**
 * API for functions specifically relating to App management.
 * 
 * @author jducoeur
 */
trait AppsFunctions {
  import AppsFunctions._
  
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
   * Returns the path to a progress monitor, which should be fed to getProgress() for regular updates.
   */
  def extractApp(elements:Seq[TID]):Future[String]
  
  /**
   * Fetch the current progress towards extraction. We recommend that the client call this about
   * once per second while extracting.
   * 
   * This is quite similar to ImportSpaceFunctions.getImportProgress(), and may want to get merged with
   * it eventually. The only real question is whether we need to abstract out the "final information" part,
   * which might be different depending on what we're monitoring. 
   */
  def getProgress(extractor:String):Future[Unit]
  
  /**
   * The client should call acknowledgeComplete after it receives an ExtractAppProgress
   * with the appInfo set. This tells the server that the client knows the
   * extraction is done, and that it can shut down and clean up.
   */
  def acknowledgeComplete(uploader:String):Unit
}

object AppsFunctions {
  case class ExtractAppProgress(msg:String, progress:Int, appInfo:Option[SpaceInfo], failed:Boolean = false)
}
