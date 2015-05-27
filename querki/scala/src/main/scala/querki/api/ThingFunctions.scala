package querki.api

import scala.concurrent.Future

import models.{DisplayText, Kind, Wikitext}
import querki.data._
import querki.pages.ThingPageDetails

trait ThingFunctions {
  /**
   * Fetch the initial info for showing the Client.
   */
  def getRequestInfo():RequestInfo
  
  /**
   * Fetch the info for the specified Thing.
   * 
   * Note that, if the named Thing does not exist, that is *not* an error: this will interpret that
   * as a Tag instead.
   */
  def getThingPage(thingId:TID, renderPropId:Option[TID]):ThingPageDetails
  
  /**
   * Fetch just the ThingInfo for the specified Thing.
   */
  def getThingInfo(thingId:TID):ThingInfo
  
  /**
   * Evaluate the given QL in the context of the specified Thing, and return the resulting Wikitext.
   */
  def evaluateQL(thingId:TID, ql:String):Wikitext
  
  /**
   * Fetch the raw values of the Properties on this Thing.
   */
  def getProperties(thingId:TID):Seq[PropValInfo]
  
  /**
   * Fetches the rendered value of the specified Property of this Thing.
   */
  def getPropertyDisplay(thingId:TID, propId:TID):Option[Wikitext]
  
  /**
   * Fetch all of the Properties available to this Space.
   */
  def getAllProperties():SpaceProps
  
  /**
   * Fetch all of the Types, Collections and Models available for creating Properties in this Space.
   */
  def getAllTypes():AllTypeInfo
  
  /**
   * Delete the specified Thing.
   */
  def deleteThing(thingId:TID):Future[Unit]
  
  /**
   * Returns the number of Instances there are for this Model.
   */
  def getNumInstances(modelId:TID):Int
}
