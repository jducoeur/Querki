package querki.display.input

import scala.concurrent.Future

import querki.editing.EditFunctions.{PropertyChangeResponse, PropertyNotChangedYet}
import querki.globals._

trait NoAutoSave { self: InputGadget[_] =>
  override def save(): Future[PropertyChangeResponse] = Future.successful(PropertyNotChangedYet)
  override def beginChanges(): Unit = {}
}
