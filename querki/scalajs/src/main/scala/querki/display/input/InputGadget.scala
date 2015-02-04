package querki.display.input

import scala.util.{Failure, Success}
import scala.scalajs.js
import js.UndefOr
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
abstract class InputGadget[T <: dom.Element](e:Ecology) extends Gadget[T] with EcologyMember {
  
  implicit val ecology = e
  
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
    // Don't hook templates! That causes nothing but havoc.
    // TODO: inputTemplate should simply go away ASAP -- it's an old approach to adding new list items.
    if (!$(elem).hasClass("inputTemplate")) {
      hook()
    }
  }
  
  // Register ourselves, so that we get hooked. Note that hooking needs to happen *after* onCreate,
  // since some libraries operate on the context we are found in:
  InputGadgetsInternal.gadgetCreated(this)
  
  /**
   * Concrete gadgets should define this. It is the current value of this Gadget, based on what's
   * on the screen.
   */
  def values:Seq[String]

  /**
   * The Thing that this input control is for. Note that this generally gets set on the server.
   * 
   * IMPORTANT: this is frequently different from DataAccess.thingId! DataAccess is telling you
   * which Thing the page is generally about, but the page can contain arbitrary edit controls
   * from other Things, via the _edit function.
   */
  lazy val thingId = $(elem).data("thing").asInstanceOf[UndefOr[String]].map(TID(_)).getOrElse(DataAccess.thingId)

  /**
   * The path to the field. This is called when we are saving. *Usually*, this is the name of the element,
   * but there are a few unfortunate inconsistencies.
   */
  def path = $(elem).attr("name")
  
  /**
   * Save the current state of this InputGadget. This can potentially be overridden, but shouldn't
   * usually require that. The Gadget should call this whenever it is appropriate to save the current
   * value.
   */
  def save():Future[PropertyChangeResponse] = {
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
    Client[EditFunctions].alterProperty(thingId, msg).call().onComplete {
      case Success(response) => {
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
      case Failure(ex) => {
        ex match {
          case querki.api.GeneralChangeFailure() => {
            StatusLine.showUntilChange(s"Something went wrong during save -- sorry!")
            $(elem).trigger("saveerror")
          }
        }
      }
    }
    promise.future
  }
}
