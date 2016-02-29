package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.pages.Page
import querki.util.{Contributor, Publisher}

/**
 * Private interface for hooking Gadgets up.
 */
private [display] trait GadgetsInternal extends EcologyInterface {
  /**
   * Each HookedGadget should register itself here, to ensure that it gets hooked.
   */
  def gadgetCreated(gadget:HookedGadget[_]):Unit
}

class GadgetsEcot(e:Ecology) extends ClientEcot(e) with Gadgets with GadgetsInternal {
  
  def implements = Set(classOf[Gadgets], classOf[GadgetsInternal])
  
  lazy val PageManager = interface[querki.display.PageManager]
  
  override def postInit() = {
    registerSimpleGadget("._withTooltip", { new WithTooltip(span()) })
    registerSimpleGadget("._qlInvoke", { new QLButtonGadget(span()) })
    registerSimpleGadget(".histogram", { new HistogramGadget })
    registerSimpleGadget("._tree", { new TreeGadget })
    registerSimpleGadget("._qlTree", { new QLTree })
    registerSimpleGadget("._menuButton", { new MenuButton })
    registerSimpleGadget("._navigateImmediately", { new NavigateGadget })
    
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
  var registry = Map.empty[String, Seq[GadgetsConstr[_]]]
  
  def registerGadgets[Output <: dom.Element](hookClass:String, constr:GadgetsConstr[Output]) = {
    registry += (registry.get(hookClass) match {
      case Some(entry) => (hookClass -> (entry :+ constr))
      case None => (hookClass -> Seq(constr))
    })
  }
  
  def registerGadget[Output <: dom.Element](hookClass:String, constr:GadgetConstr[Output]):Unit = {
    registerGadgets(hookClass, { elem:dom.Element => Seq(constr(elem)) })
  }
  
  def registerSimpleGadgets[Output <: dom.Element](hookClass:String, constr: => Seq[Gadget[Output]]):Unit = {
    val fullConstr = { e:dom.Element =>
      val gadgets = constr
      gadgets.foreach(_.setElem(e))
      gadgets
    }
    registerGadgets(hookClass, fullConstr)
  }
  
  def registerSimpleGadget[Output <: dom.Element](hookClass:String, constr: => Gadget[Output]):Unit =
    registerSimpleGadgets(hookClass, { Seq(constr) })
  
  def registerHook(selector:String)(hook:dom.Element => Unit) = {
    registerSimpleGadgets(selector, { Seq(new HookGadget(hook)) })
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
  
  var unhookedGadgets = Set.empty[HookedGadget[_]]
  
  def gadgetCreated(gadget:HookedGadget[_]) =
    unhookedGadgets += gadget
  
  def hookPendingGadgets() = {
    // Only hook gadgets that have actually been created!
    val (pending, unready) = unhookedGadgets.partition { g => g.elemOpt.isDefined }
    // What's going on here? We need to allow for InputGadgets whose hook creates
    // *more* InputGadgets. So we deal with this list, then check whether more got
    // created along the way:
    unhookedGadgets = Set.empty
    pending.foreach(_.prep())
    if (!unhookedGadgets.isEmpty) {
      // Recurse to do more:
      hookPendingGadgets()
    }
    unhookedGadgets ++= unready
  }
}
