package querki.display

import scala.scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.pages.Page
import querki.util.{Contributor, Publisher}

class GadgetsEcot(e:Ecology) extends ClientEcot(e) with Gadgets {
  
  def implements = Set(classOf[Gadgets])
  
  lazy val PageManager = interface[querki.display.PageManager]
  
  override def postInit() = {
    registerSimpleGadget("._withTooltip", { new WithTooltip(span()) })
    registerSimpleGadget("._qlInvoke", { new QLButtonGadget(span()) })
    
    PageManager.beforePageLoads += new Contributor[Page,Unit] {
      def notify(evt:Page, sender:Publisher[Page, Unit]) = {
        registry = mainRegistry
      }
    }    
  }
  
  // Pay careful attention to this -- it first gets hit during the first page load, at which
  // point it gets initialized to the registry as it was from postInit(). It is then used to
  // refresh the registry each page load, so that pages can add their own registrations.
  lazy val mainRegistry = registry
  
  /**
   * The actual registry of all of the Gadgets. This is a map from the name of the marker
   * class for this Gadget to a factory function for it. 
   */
  var registry = Map.empty[String, GadgetConstr]
  
  def registerGadget(hookClass:String, constr:GadgetConstr) = {
    registry += (hookClass -> constr)
  }
  
  def registerSimpleGadget(hookClass:String, constr: => Gadget[_]) = {
    val fullConstr = { e:dom.Element =>
      val gadget = constr
      gadget.setElem(e)
      gadget
    }
    registerGadget(hookClass, fullConstr)
  }
  
  def registerHook(selector:String)(hook:dom.Element => Unit) = {
    registerSimpleGadget(selector, { new HookGadget(hook) })
  }
  
  def createGadgets(root:dom.Element) = {
    registry.foreach { pair =>
      val (className, constr) = pair
      if ($(root).is(s"$className")) { constr(root) }
      $(root).find(s"$className").each({ (elem:dom.Element) =>
        constr(elem)
      }:js.ThisFunction0[dom.Element, Any])
    }
  }
}