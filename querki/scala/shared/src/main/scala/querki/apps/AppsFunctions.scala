package querki.apps

import scala.concurrent.Future

import querki.api.OperationHandle
import querki.data._

/**
 * API for functions specifically relating to App management.
 * 
 * @author jducoeur
 */
trait AppsFunctions {
  import AppsFunctions._
  
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
   */
  def extractApp(elements:Seq[TID], name:String):Future[Unit]
  
  /**
   * Fetches the list of Models that are available to Extract from this Space.
   */
  def getExtractableModels():Future[Seq[ExtractableModelInfo]]
}

object AppsFunctions {
  case class ExtractableModelInfo(
    oid:TID, 
    linkName:Option[String], 
    displayName:String,
    canExtract:Boolean,
    extractInstancesByDefault:Boolean
  ) extends BasicThingInfo
}
