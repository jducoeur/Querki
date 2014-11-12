package querki.display.input

import upickle._
import autowire._

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.display._

/**
 * Base class for input controls. When you create a new concrete class, make sure to add it to
 * InputGadgets.registry.
 */
abstract class InputGadget[T <: dom.Element](val ecology:Ecology) extends Gadget[T] with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val InputGadgetsInternal = interface[InputGadgetsInternal]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  /**
   * Hook whatever events are appropriate for this Gadget.
   */
  protected def hook():Unit

  /**
   * Called by InputGadgets when it is time to prepare this Gadget for the world.
   */
  def prep() = {
    hook()
  }
  
  // Register ourselves, so that we get hooked. Note that hooking needs to happen *after* onCreate,
  // since some libraries operate on the context we are found in:
  InputGadgetsInternal.gadgetCreated(this)
  
  /**
   * Concrete gadgets should define this. It is the current value of this Gadget, based on what's
   * on the screen.
   */
  def values:List[String]
  
  /**
   * Save the current state of this InputGadget. This can potentially be overridden, but shouldn't
   * usually require that. The Gadget should call this whenever it is appropriate to save the current
   * value.
   */
  def save():Future[PropertyChangeResponse] = {
    val path = $(elem).attr("name")
    saveChange(ChangePropertyValue(path, values))
  }

  /**
   * InputGadgets should call this iff they have a complex edit cycle -- that is, if you begin to
   * edit and later save those changes. Use this to make sure that changes get saved before we navigate.
   * 
   * If you call this, you should also define the save() method.
   */
  def beginChanges() = InputGadgetsInternal.startingEdits(this)
  
  /**
   * Records a change that the user has made. This should be called by the specific Gadget when
   * appropriate.
   * 
   * @param msg The change event to send to the server.
   */
  def saveChange(msg:PropertyChange):Future[PropertyChangeResponse] = {
    StatusLine.showUntilChange("Saving...")
    val promise = Promise[PropertyChangeResponse]
    Client[EditFunctions].alterProperty(DataAccess.thingId, msg).call().foreach { response =>
      InputGadgetsInternal.saveComplete(this)
	  response match {
        case PropertyChanged => {
          StatusLine.showBriefly("Saved")
          $(elem).trigger("savecomplete")
        }
        case PropertyChangeError(msg) => {
          StatusLine.showUntilChange(s"Error: $msg")
          $(elem).trigger("saveerror")
        }
      }
      promise.success(response)
    }
    promise.future
  }
}
