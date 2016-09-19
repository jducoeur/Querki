package querki.api

import scala.concurrent.Future

import models.{DisplayText, Kind, Wikitext}
import querki.data._
import querki.pages.ThingPageDetails

trait ThingFunctions {
  /**
   * Fetch the initial info for showing the Client.
   */
  def getRequestInfo():Future[RequestInfo]
  
  /**
   * Fetch the info for the specified Thing.
   * 
   * Note that, if the named Thing does not exist, that is *not* an error: this will interpret that
   * as a Tag instead.
   */
  def getThingPage(thingId:TID, renderPropId:Option[TID]):Future[ThingPageDetails]
  
  /**
   * Fetch just the ThingInfo for the specified Thing.
   */
  def getThingInfo(thingId:TID):Future[ThingInfo]
  
  /**
   * Evaluate the given QL in the context of the specified Thing, and return the resulting Wikitext.
   */
  def evaluateQL(thingId:TID, ql:String):Future[Wikitext]
  
  /**
   * Evaluate the given QL with an arbitrary context, and return the resulting Wikitext. 
   */
  def evaluateQLWithContext(typeId:TID, context:String, lexical:Option[TID], ql:String):Future[Wikitext]
  
  /**
   * Fetch the raw values of the Properties on this Thing.
   */
  def getProperties(thingId:TID):Future[Seq[PropValInfo]]
  
  /**
   * Fetches the rendered value of the specified Property of this Thing.
   */
  def getPropertyDisplay(thingId:TID, propId:TID):Future[Option[Wikitext]]
  
  /**
   * Fetch all of the Properties available to this Space.
   */
  def getAllProperties():SpaceProps
  
  /**
   * Fetch all of the Types, Collections and Models available for creating Properties in this Space.
   */
  def getAllTypes():Future[AllTypeInfo]
  
  /**
   * Delete the specified Thing.
   * 
   * TODO: this doesn't belong here! This really belongs in EditFunctions, or something like that.
   */
  def deleteThing(thingId:TID):Future[Unit]
  
  /**
   * Returns the number of Instances there are for this Model.
   */
  def getNumInstances(modelId:TID):Int
  
  /**
   * Returns the *immediate* children of the specified Model.
   */
  def getChildren(modelId:TID, includeModels:Boolean, includeInstances:Boolean):Future[Seq[ThingInfo]]
  
  /**
   * Reloads this Space. Mainly intended for testing.
   */
  def reloadSpace():Future[Unit]
}
