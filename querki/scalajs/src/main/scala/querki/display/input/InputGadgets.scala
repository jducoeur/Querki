package querki.display.input

import scala.scalajs.js
import js.ThisFunction._
import org.scalajs.dom

import querki.globals._

import querki.display.Gadget

/**
 * Private interface, allowing InputGadgets to work with their master controller.
 */
private [input] trait InputGadgetsInternal extends EcologyInterface {
    /**
     * Each InputGadget should register itself here, to ensure that it gets hooked.
     */
    def gadgetCreated(gadget:InputGadget[_]):Unit
    
    /**
     * Record that this Gadget has begun to be edited, and is not yet saved. Use this for complex Gadgets
     * that don't simply save immediately on every change, so that we can force-save when needed.
     */
    def startingEdits(gadget:InputGadget[_]):Unit
    
    /**
     * The pair to startingEdits(), which should be called when save is complete.
     */
    def saveComplete(gadget:InputGadget[_]):Unit
}

class InputGadgetsEcot(e:Ecology) extends ClientEcot(e) with InputGadgets with InputGadgetsInternal {
  
  def implements = Set(classOf[InputGadgets], classOf[InputGadgetsInternal])
  
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  override def postInit() = {
    Gadgets.registerSimpleGadget("._textEdit", { new TextInputGadget(Seq.empty) })
    Gadgets.registerSimpleGadget("._largeTextEdit", { new LargeTextInputGadget })
    Gadgets.registerGadget("._tagSetInput", { TagSetInput(_) })
    Gadgets.registerGadget("._tagInput", { MarcoPoloInput(_) })
    // TODO: this ought to start with an underscore:
    Gadgets.registerSimpleGadget(".sortableList", { new SortableListGadget })
    // Note that we currently assume all selects are inputs:
    Gadgets.registerSimpleGadget("select", { new SelectGadget })
    Gadgets.registerGadget("._deleteInstanceButton", { DeleteInstanceButton(_) })
    Gadgets.registerSimpleGadget("._rating", { new RatingGadget })
    Gadgets.registerSimpleGadget("._optYesNo", { new OptYesNoGadget })
    Gadgets.registerGadgets(".propEditor", hookOtherPropEditor)
  }
  
  /**
   * This is the catch-all for hooking editors that don't have a simple class name attached.
   * 
   * TODO: this probably should be a general registry itself, based on the collection and type of the
   * editor.
   */
  def hookOtherPropEditor[Output <: dom.Element](elem:Output):Seq[Gadget[Output]] = {
    val e = $(elem)
    val tagName = e.prop("tagName").asInstanceOf[String].toLowerCase
    val tagType = e.jqf.attr("type")
    val results = if ((tagName == "input") && (tagType.isDefined) && (tagType.get == "checkbox")) {
      Seq((new CheckboxGadget).asInstanceOf[Gadget[Output]])
    } else
      Seq.empty
    results.map(_.setElem(elem))
    results
  }
  
  var unhookedGadgets = Set.empty[InputGadget[_]]
  
  def gadgetCreated(gadget:InputGadget[_]) =
    unhookedGadgets += gadget
  
  def hookPendingGadgets() = {
    // What's going on here? We need to allow for InputGadgets whose hook creates
    // *more* InputGadgets. So we deal with this list, then check whether more got
    // created along the way:
    val pending = unhookedGadgets
    unhookedGadgets = Set.empty
    pending.foreach(_.prep())
    if (!unhookedGadgets.isEmpty)
      // Recurse to do more:
      hookPendingGadgets()
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
      promise.success(())
    }
  }
  
  def afterAllSaved:Future[Unit] = {
    if (gadgetsBeingEdited.isEmpty)
      Future.successful(())
    else {
      val promise = Promise[Unit]
      savePromise = Some(promise)
      gadgetsBeingEdited.foreach(_.save())
      promise.future
    }
  }
}
