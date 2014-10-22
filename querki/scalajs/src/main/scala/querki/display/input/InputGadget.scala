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
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  /**
   * Hook whatever events are appropriate for this Gadget.
   */
  def hook():Unit
  
  /**
   * If this InputGadget gets created in the conventional way, hook it after it's created.
   * 
   * Note that InputGadgets tend to come from two different sources. Sometimes they are built
   * by QText -- that is, they are defined in Wikitext -- in which case InputGadgets learns
   * about them *after* they are built, and then hooks them. OTOH, if they are specified in
   * Scalatags and then rendered, this will be called, so we can hook them.
   */
  override def onCreate(elem:T) = hook() 
  
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
