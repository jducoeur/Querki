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
  lazy val InputGadgets = interface[InputGadgets]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  /**
   * Hook whatever events are appropriate for this Gadget.
   */
  def hook():Unit
  
  // Register ourselves, so that we get hooked. Note that hooking needs to happen *after* onCreate,
  // since some libraries operate on the context we are found in:
  InputGadgets.gadgetCreated(this)
  
  /**
   * Save the current state of this InputGadget. Use this iff you are using beginChanges().
   */
  def save():Unit = {}

  /**
   * InputGadgets should call this iff they have a complex edit cycle -- that is, if you begin to
   * edit and later save those changes. Use this to make sure that changes get saved before we navigate.
   * 
   * If you call this, you should also define the save() method.
   */
  def beginChanges() = InputGadgets.startingEdits(this)
  
  /**
   * Records a change that the user has made. This should be called by the specific Gadget when
   * appropriate.
   * 
   * @param vs The new values of this field. Note that this is plural, since some Gadgets are inherently
   *    List/Set based. Conventional single-valued fields should just pass in Some(v). Values should be
   *    in whatever serialized form the server-side PType expects.
   */
  def saveChange(vs:List[String]) = {
    StatusLine.showUntilChange("Saving...")
    val path = $(elem).attr("name")
    Client[EditFunctions].alterProperty(DataAccess.thingId, path, ChangePropertyValue(vs)).call().foreach { response =>
      InputGadgets.saveComplete(this)
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
    }
  }
}
