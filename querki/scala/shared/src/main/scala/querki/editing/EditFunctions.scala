package querki.editing

import scala.concurrent.Future
import models.Wikitext
import querki.data._
import EditFunctions.FullEditInfo
import EditFunctions.PropEditInfo
import EditFunctions.PropertyChange
import EditFunctions.PropertyChangeResponse

trait EditFunctions {
  import EditFunctions._
  /**
   * The central "edit" action: change the presence or content of one Property on a Bundle.
   */
  def alterProperty(thingId:TID, change:PropertyChange):Future[PropertyChangeResponse]
  
  /**
   * Create a new Thing with the given Model and properties.
   */
  def create(modelId:TID, initialProps:Seq[PropertyChange]):Future[ThingInfo]
  
  /**
   * Fetch the Editor for the specified Property on this Thing. The Property does not have to
   * be defined on the Thing yet.
   */
  def getOnePropertyEditor(thingId:TID, propId:TID):PropEditInfo
  
  /**
   * Fetch the Editors and ancillary information about this Thing.
   */
  def getPropertyEditors(thingId:TID):FullEditInfo
  
  /**
   * Add this Property to this Thing, and return the Editor for it.
   */
  def addPropertyAndGetEditor(thingId:TID, propId:TID):Future[PropEditInfo]
  
  /**
   * Remove the specified Property from the specified Thing.
   */
  def removeProperty(thingId:TID, propId:TID):Future[PropertyChangeResponse]
  
  /**
   * Fetch a Type based on this Model, creating it if necessary.
   */
  def getModelType(modelId:TID):Future[TypeInfo]
  
  /**
   * Changes the Model that this Thing is based on.
   */
  def changeModel(thingId:TID, newModel:TID):Future[ThingInfo]
  
  /**
   * Fetch the Large Text value used to reify Tags for this Model.
   */
  def getUndefinedTagView(modelId:TID):String
}

object EditFunctions {
  sealed abstract trait PropertyChange {
    // The path to the Property being altered:
    def path:String
  }
  
  /**
   * Describes a changed value on a Property. Note that the values are a List, to be able to support
   * Optional, List and Set.
   */
  case class ChangePropertyValue(path:String, currentValues:Seq[String]) extends PropertyChange
  
  case class MoveListItem(path:String, from:Int, to:Int) extends PropertyChange
  
  case class AddListItem(path:String) extends PropertyChange
  
  case class DeleteListItem(path:String, index:Int) extends PropertyChange
  
  case class AddToSet(path:String, value:String) extends PropertyChange
  case class RemoveFromSet(path:String, value:String) extends PropertyChange
  
  sealed trait PropertyChangeResponse
  case object PropertyChanged extends PropertyChangeResponse
  
  case class FullEditInfo(instancePropIds:Seq[TID], instancePropPath:String, derivingName:Boolean, propInfos:Seq[PropEditInfo])
  
  case class PropEditInfo(
    propInfo:PropInfo,
	path:String,
	prompt:Option[Wikitext],
    tooltip:Option[Wikitext],
	inheritedFrom:Option[String],
	canEditProperty:Boolean,
	// This is the raw HTML for the Editor
	editor:String
  )
}
