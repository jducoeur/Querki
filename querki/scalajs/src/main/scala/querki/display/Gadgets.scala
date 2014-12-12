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
  // TODO: this approach is fundamentally dangerous: if we ever get to the point of, eg, showing
  // two Pages side-by-side, or nested, this global information is going to kill us. Arguably, the
  // current working registry should *belong* to the Page, layering its own stuff on top of the
  // mainRegistry, so that multiple Pages can work in parallel without bothering each other.
  // But that will require that the entire Gadget chain be able to trace back to its containing Page:
  // a good change, but a big one.
  lazy val mainRegistry = registry
  
  /**
   * The actual registry of all of the Gadgets. This is a map from a Selector to the constructors
   * of the Gadgets looking for that Selector.
   */
  var registry = Map.empty[String, Seq[GadgetConstr]]
  
  def registerGadget(hookClass:String, constr:GadgetConstr) = {
    registry += (registry.get(hookClass) match {
      case Some(entry) => (hookClass -> (entry :+ constr))
      case None => (hookClass -> Seq(constr))
    })
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
      val (className, constrs) = pair
      if ($(root).is(s"$className")) { constrs.map(_(root)) }
      $(root).find(s"$className").each({ (elem:dom.Element) =>
        constrs.map(_(elem))
      }:js.ThisFunction0[dom.Element, Any])
    }
  }
}
