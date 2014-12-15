package querki.api

import scala.concurrent.Future

import models.Wikitext
import querki.data._

trait EditFunctions {
  import EditFunctions._
  /**
   * The central "edit" action: change the presence or content of one Property on a Bundle.
   */
  def alterProperty(thingId:String, change:PropertyChange):Future[PropertyChangeResponse]
  
  /**
   * Create a new Thing with the given Model and properties.
   */
  def create(modelId:String, initialProps:Seq[PropertyChange]):Future[ThingInfo]
  
  /**
   * Fetch the Editor for the specified Property on this Thing. The Property does not have to
   * be defined on the Thing yet.
   */
  def getOnePropertyEditor(thingId:String, propId:String):PropEditInfo
  
  /**
   * Fetch the Editors and ancillary information about this Thing.
   */
  def getPropertyEditors(thingId:String):FullEditInfo
  
  /**
   * Add this Property to this Thing, and return the Editor for it.
   */
  def addPropertyAndGetEditor(thingId:String, propId:String):Future[PropEditInfo]
  
  /**
   * Remove the specified Property from the specified Thing.
   */
  def removeProperty(thingId:String, propId:String):Future[PropertyChangeResponse]
  
  /**
   * Fetch a Type based on this Model, creating it if necessary.
   */
  def getModelType(modelId:String):Future[TypeInfo]
}

object EditFunctions {
  sealed abstract trait PropertyChange
  case object DeleteProperty extends PropertyChange
  /**
   * Describes a changed value on a Property. Note that the values are a List, to be able to support
   * Optional, List and Set.
   */
  case class ChangePropertyValue(path:String, currentValues:Seq[String]) extends PropertyChange
  
  case class MoveListItem(path:String, from:Int, to:Int) extends PropertyChange
  
  case class AddListItem(path:String) extends PropertyChange
  
  case class DeleteListItem(path:String, index:Int) extends PropertyChange
  
  sealed trait PropertyChangeResponse
  case object PropertyChanged extends PropertyChangeResponse
  case class PropertyChangeError(msg:String) extends PropertyChangeResponse
  
  case class FullEditInfo(instancePropIds:Seq[String], instancePropPath:String, propInfos:Seq[PropEditInfo])
  
  case class PropEditInfo(
    propInfo:PropInfo,
	path:String,
	prompt:Option[Wikitext],
    tooltip:Option[Wikitext],
	inheritedFrom:Option[String],
	// This is the raw HTML for the Editor
	editor:String
  )
}
