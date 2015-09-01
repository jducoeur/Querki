package querki.editing

import scala.concurrent.Future

import querki.api.OperationHandle
import querki.data._

class EditFunctionsEmpty extends EditFunctions {
  import EditFunctions._
  
  def alterProperty(thingId:TID, change:PropertyChange):Future[PropertyChangeResponse] = ???
  def create(modelId:TID, initialProps:Seq[PropertyChange]):Future[ThingInfo] = ???
  def getOnePropertyEditor(thingId:TID, propId:TID):PropEditInfo = ???
  def getPropertyEditors(thingId:TID):FullEditInfo = ???
  def addPropertyAndGetEditor(thingId:TID, propId:TID):Future[PropEditInfo] = ???
  def removeProperty(thingId:TID, propId:TID):Future[PropertyChangeResponse] = ???
  def getModelType(modelId:TID):Future[TypeInfo] = ???
  def changeModel(thingId:TID, newModel:TID):Future[ThingInfo] = ???
  def getUndefinedTagView(modelId:TID):String = ???
  def getPropertyUsage(propId:TID):PropUsage = ???
  def removePropertyFromAll(propId:TID):OperationHandle = ???
}