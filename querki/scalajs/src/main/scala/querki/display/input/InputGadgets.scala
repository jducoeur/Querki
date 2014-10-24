package querki.display.input

import scala.scalajs.js
import js.ThisFunction._
import org.scalajs.dom

import querki.globals._

class InputGadgetsEcot(e:Ecology) extends ClientEcot(e) with InputGadgets {
  
  def implements = Set(classOf[InputGadgets])
  
  /**
   * The factory function for an InputGadget. It is consistent and trivial, but we don't have
   * reflection here, so can't just automate it.
   */
  type InputConstr = (dom.Element => InputGadget[_])
  
  /**
   * The actual registry of all of the InputGadgets. This is a map from the name of the marker
   * class for this InputGadget to a factory function for it. 
   * 
   * The coupling here is a bit unfortunate, but
   * seems to be the least boilerplatey way I can think of to do things, given that we don't have
   * reflection (and thus, dynamic construction) on the client side.
   */
  val registry = Map[String, InputConstr](
    ("_textEdit" -> { TextInputGadget(_) }),
    ("_largeTextEdit" -> { LargeTextInputGadget(_) }),
    ("_tagSetInput" -> { TagSetInput(_) }),
    ("_tagInput" -> { MarcoPoloInput(_) })
  )
  
  var unhookedGadgets = Set.empty[InputGadget[_]]
  
  def gadgetCreated(gadget:InputGadget[_]) =
    unhookedGadgets += gadget
  
  val jsUnit = 1:js.Any
  
  def hookPendingGadgets() = {
    unhookedGadgets.foreach(_.hook())
    unhookedGadgets = Set.empty
  }
  
  def hookRawGadgets(root:dom.Element) = {
    registry.foreach { pair =>
      val (className, constr) = pair
      // TODO: this is the old signature of .each(). Replace this with a more modern version:
      $(root).find(s".$className").each ({ (index:js.Any, elem:dom.Element) =>
        val gadget = constr(elem)
        gadget.hook()
        unhookedGadgets -= gadget
        jsUnit
      })
    }
  }
  
  /**
   * Gadgets that are currently being edited, which haven't yet been saved.
   */
  var gadgetsBeingEdited = Set.empty[InputGadget[_]]
  
  var savePromise:Option[Promise[Unit]] = None
  
  def startingEdits(gadget:InputGadget[_]) = {
    gadgetsBeingEdited += gadget
  }
  def saveComplete(gadget:InputGadget[_]) = {
    gadgetsBeingEdited -= gadget
    if (gadgetsBeingEdited.isEmpty && savePromise.isDefined) {
      val promise = savePromise.get
      savePromise = None
      promise.success()
    }
  }
  
  def afterAllSaved:Future[Unit] = {
    if (gadgetsBeingEdited.isEmpty)
      Future.successful()
    else {
      val promise = Promise[Unit]
      savePromise = Some(promise)
      gadgetsBeingEdited.foreach(_.save())
      promise.future
    }
  }
}
