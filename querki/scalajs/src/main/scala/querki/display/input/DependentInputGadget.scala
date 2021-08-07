package querki.display.input

import org.scalajs.dom.html.Element

import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.display.{HookedGadget, RawSpan, ServerHtmlHolder}
import querki.editing.EditFunctions
import querki.globals._

/**
 * Despite the name, this isn't an InputGadget per se. Instead, it's a Gadget that you *attach* to
 * an InputGadget, saying that it is dependent on another one. When the other one changes, this one
 * gets reloaded.
 */
class DependentInputGadget(implicit e: Ecology) extends HookedGadget[Element](e) with ServerHtmlHolder {

  lazy val Client = interface[querki.client.Client]
  lazy val Pages = interface[querki.pages.Pages]

  // data-dependson is required for _depends objects:
  lazy val dependsOnPath = $(elem).dataString("dependson")
  lazy val thing = $(elem).dataString("thing")
  lazy val prop = $(elem).dataString("prop")

  def dependencyUpdated(): Unit = {
    // The element we depend on has changed. Therefore, reload this element, re-process it, and stick it here:
    Client[EditFunctions].getOnePropertyEditor(TID(thing), TID(prop)).call().map { editInfo =>
      Pages.findPageFor(this).foreach { page =>
        page.inputDependencies.unregister(this, dependsOnPath)
      }

      // Use JQuery to build the new node, and add it in place:
      val newJQ = $(editInfo.editor)
      val newElem = newJQ.get(0).get.asInstanceOf[Element]
      $(elem).after(newJQ)
      // HACK: make sure the replacement control has all the classes of the old one. This really isn't right; how
      // can we do it more correctly? It suggests that our pathways aren't quite correct, since it's not reliably
      // coming from getOnePropertyEditor() with all the right bits.
      newElem.setAttribute("class", elem.getAttribute("class"))
      $(elem).remove()
      prepContents(newElem)
      Gadgets.hookPendingGadgets()
    }
  }

  def hook() = {
    // Find the page, and register ourself as a listener for the target path:
    Pages.findPageFor(this).foreach { page =>
      page.inputDependencies.register(this, dependsOnPath)
    // No, we're not detecting removal. This turns out to be hard-to-impossible in the
    // standard DOM, so we're breaking cycles in InputDependencies itself.
    }
  }

  def doRender = ???
}
