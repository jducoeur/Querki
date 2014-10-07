package querki.api

trait EditFunctions {
  import EditFunctions._
  /**
   * The central "edit" action: change the presence or content of one Property on a Bundle.
   */
  def alterProperty(thingId:String, path:String, change:PropertyChange):PropertyChangeResponse
}

object EditFunctions {
  sealed abstract trait PropertyChange
  case object DeleteProperty extends PropertyChange
  /**
   * Describes a changed value on a Property. Note that the values are a List, to be able to support
   * Optional, List and Set.
   */
  case class ChangePropertyValue(currentValues:List[String]) extends PropertyChange
  
  sealed trait PropertyChangeResponse
  case object PropertyChanged extends PropertyChangeResponse
  case class PropertyChangeError(msg:String) extends PropertyChangeResponse
}
