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
  def getThingPage(thingId:String):ThingPageDetails
  
  /**
   * Fetch just the ThingInfo for the specified Thing.
   */
  def getThingInfo(thingId:String):ThingInfo
  
  /**
   * Evaluate the given QL in the context of the specified Thing, and return the resulting Wikitext.
   */
  def evaluateQL(thingId:String, ql:String):Wikitext
  
  /**
   * Fetch the raw values of the Properties on this Thing.
   */
  def getProperties(thingId:String):Seq[PropValInfo]
  
  /**
   * Fetch all of the Properties available to this Space.
   */
  def getAllProperties():SpaceProps
  
  /**
   * Delete the specified Thing.
   */
  def deleteThing(thingId:String):Future[Unit]
  
  /**
   * Returns the number of Instances there are for this Model.
   */
  def getNumInstances(modelId:String):Int
}
