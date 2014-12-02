package querki.display

import scala.scalajs.js
import org.scalajs.dom

import querki.globals._

class GadgetsEcot(e:Ecology) extends ClientEcot(e) with Gadgets {
  
  def implements = Set(classOf[Gadgets])
  
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
  
  def createGadgets(root:dom.Element) = {
    registry.foreach { pair =>
      val (className, constr) = pair
      $(root).find(s"$className").each({ (elem:dom.Element) =>
        val gadget = constr(elem)
      }:js.ThisFunction0[dom.Element, Any])
    }
  }
}