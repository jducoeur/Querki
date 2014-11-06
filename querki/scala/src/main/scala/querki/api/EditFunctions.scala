package querki.api

import scala.concurrent.Future

import querki.data.ThingInfo

trait EditFunctions {
  import EditFunctions._
  /**
   * The central "edit" action: change the presence or content of one Property on a Bundle.
   */
  def alterProperty(thingId:String, change:PropertyChange):PropertyChangeResponse
  
  /**
   * Create a new Thing with the given Model and properties.
   */
  def create(modelId:String, initialProps:Seq[PropertyChange]):Future[ThingInfo]
}

object EditFunctions {
  sealed abstract trait PropertyChange
  case object DeleteProperty extends PropertyChange
  /**
   * Describes a changed value on a Property. Note that the values are a List, to be able to support
   * Optional, List and Set.
   */
  case class ChangePropertyValue(path:String, currentValues:List[String]) extends PropertyChange
  
  sealed trait PropertyChangeResponse
  case object PropertyChanged extends PropertyChangeResponse
  case class PropertyChangeError(msg:String) extends PropertyChangeResponse
}
