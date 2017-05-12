package querki.display.input

import scala.util.{Failure, Success}
import scala.scalajs.js
import js.UndefOr

import upickle._
import autowire._
import org.scalajs.dom
import dom.html.Element
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._
import querki.data.BasicThingInfo
import querki.display._
import querki.display.HookedGadget
import querki.editing.EditFunctions
import EditFunctions._

/**
 * Base class for input controls. When you create a new concrete class, make sure to add it to
 * InputGadgets.registry.
 */
abstract class InputGadget[T <: Element](e:Ecology) extends HookedGadget[T](e) with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]  
  lazy val InputGadgetsInternal = interface[InputGadgetsInternal]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  /**
   * Iff this Gadget allows its value to be changed from the outside, set the value.
   * 
   * TODO: this is pretty much a hack, mainly in place for CreateAndEditPage's setValue().
   * It's clearly undertyped and wrong. What's the right approach here? In principle, it seems
   * like each Gadget type should optionally expose the type of its value, which we should use
   * here. But I'd prefer to not have to specify that explicit for each and every Gadget. Is
   * there a way to have a "default type" in the type parameters?
   */
  def setValue(v:String):Unit = {}
  
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
  def path = $(elem).attr("name").get
  
  private def isColl(propColl:String, coll:BasicThingInfo):Option[BasicThingInfo] = {
    if (propColl == coll.oid.underlying)
      Some(coll)
    else
      None
  }
  /**
   * Returns the Collection for this Property.
   * 
   * This must only be called after the InputGadget is fully set up! (Usually in hook().)
   */
  def propCollection:BasicThingInfo = {
    val propColl = $(elem).dataString("collid")
    val core = DataAccess.std.core
    isColl(propColl, core.optionalColl).getOrElse(
      isColl(propColl, core.exactlyOneColl).getOrElse(
      isColl(propColl, core.listColl).getOrElse(
      isColl(propColl, core.setColl).getOrElse(
      throw new Exception(s"Property has unknown collection $propColl")))))
  }
  /**
   * True iff this Property is Optional.
   */
  def isOptional:Boolean = {
    val propColl = $(elem).dataString("collid")
    val core = DataAccess.std.core
    isColl(propColl, core.optionalColl).isDefined
  }
  
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
  def beginChanges() = {
    InputGadgetsInternal.startingEdits(this)
  }
  
  var errorWrapper:Option[dom.html.Div] = None
  def showValidationError(msg:String) = {
    val wrapper =
      div(cls:="_validationError",
        p(msg)
      ).render
    $(wrapper).insertBefore(elem)
    $(elem).detach()
    $(wrapper).prepend(elem)
    $(elem).focus()
    
    errorWrapper = Some(wrapper)
  }
  def clearValidationError() = {
    errorWrapper.foreach { wrapper =>
      $(elem).detach()
      $(wrapper).replaceWith(elem)
      errorWrapper = None
    }
  }
  
  /**
   * Records a change that the user has made. This should be called by the specific Gadget when
   * appropriate.
   * 
   * @param msg The change event to send to the server.
   */
  def saveChange(msg:PropertyChange):Future[PropertyChangeResponse] = {
    val fut = InputGadget.doSaveChange(thingId, msg)
    fut.onComplete {
      case Success(response) => {
        InputGadgetsInternal.saveComplete(this)
	      response match {
          case PropertyChanged => {
            $(elem).trigger("savecomplete")
          }
        }
      }
      
      case Failure(ex) => {
        ex match {
          case querki.api.ValidationException(msg) => {
            showValidationError(msg)
            $(elem).trigger("saveerror")
          }
          case querki.api.GeneralChangeFailure(msg) => {
            $(elem).trigger("saveerror")
          }
        }
      }
    }
    fut
  }
}

object InputGadget {
  /**
   * The guts of saving changes, pulled out of InputGadget so that other sorts of controls can
   * make use of it.
   */
  def doSaveChange(thingId:TID, msg:PropertyChange)(implicit ecology:Ecology):Future[PropertyChangeResponse] = {
    val Client = ecology.api[querki.client.Client]
    val InputGadgetsInternal = ecology.api[InputGadgetsInternal]
    val StatusLine = ecology.api[querki.display.StatusLine]
    
    StatusLine.showUntilChange("Saving...")
    val promise = Promise[PropertyChangeResponse]
    Client[EditFunctions].alterProperty(thingId, msg).call().onComplete {
      case Success(response) => {
	      response match {
          case PropertyChanged => {
            StatusLine.showBriefly("Saved")
          }
        }
        promise.success(response)
      }
      case Failure(ex) => {
        ex match {
          case querki.api.ValidationException(msg) => {
            StatusLine.clear()
          }
          case querki.api.GeneralChangeFailure(msg) => {
            StatusLine.showUntilChange(msg)
          }
        }
      }
    }
    promise.future
  }
}
