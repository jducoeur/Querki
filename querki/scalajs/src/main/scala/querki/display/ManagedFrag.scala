package querki.display

import scala.annotation.tailrec

import scala.scalajs.js
import js.UndefOr
import scalatags.JsDom.all._
import org.scalajs.dom
import org.querki.jquery._
import _root_.rx._

import querki.globals._

/**
 * A controlled wrapper around a Scalatags Frag, which gives you access to the lifecycle and
 * the resulting DOM objects.
 * 
 * In and of itself, ManagedFrag is a bit weak, but it is the basis of multiple forms of
 * useful gadgetry.
 */
trait ManagedFrag[Output <: dom.Node] extends scalatags.jsdom.Frag {
  
  val elemOptRx = Var[Option[Output]](None)
  def elemOpt = elemOptRx()
  def elem = elemOpt.get
  
  /**
   * Slam the element for this Gadget. You should only call this iff the element was actually called from
   * an external mechanism (eg, via QText), and you're building this Gadget around that element.
   * 
   * This is intentionally designed for chaining, for ease of use -- it returns this Gadget.
   */
  def setElem(e:dom.Node):this.type = {
    elemOptRx() = Some(e.asInstanceOf[Output])
    val gadgets =
      if ($(elem).hasClass("_withGadget")) {
        val existingGadgets = $(elem).data("gadgets").asInstanceOf[UndefOr[Seq[AnyFrag]]].getOrElse(Seq.empty)
        existingGadgets :+ this
      } else {
        Seq(this)
      }
    // TODO: this should be a Seq of Gadgets, not a single one, so we can attach multiple
    // Gadgets to a single Element!
    $(elem).data("gadgets", gadgets.asInstanceOf[js.Any])
    $(elem).addClass("_withGadget")
    this
  }
  
  /**
   * Concrete classes must define this. It causes the actual DOM node to come into existence.
   */
  def createFrag:Output
  
  /**
   * Concrete Gadgets can override this to perform actions after we've created the actual Element.
   */
  def onCreate(elem:Output) = {}
  
  /**
   * We intercept render (which is part of Scalatags), to record the Node when it gets created, and
   * to provide access to the creation event.
   * 
   * IMPORTANT: this imperatively renders the Gadget; if you call it repeatedly, it will render
   * again! Use rendered by preference most of the time. 
   */
  def render = {
    val result = createFrag
    setElem(result)
    onCreate(result)
    // Note that we intentionally don't return result at this point, in case onCreate()
    // mutates elem:
    elem
  }
  
  type AnyNode <: dom.Node
  type AnyFrag = ManagedFrag[AnyNode]
  
  def findGadgetsFor(root:JQuery, pred:AnyFrag => Boolean):Seq[AnyFrag] = {
    val gadgetOptsArray = root.find("._withGadget").map({ (e:dom.Element) =>
      val frags = $(e).data("gadgets").asInstanceOf[Seq[AnyFrag]]
      frags.filter(pred(_))
    }:js.ThisFunction0[dom.Element, Any]).get()
    
    val gadgetOptsSeq:Seq[Seq[AnyFrag]] = gadgetOptsArray.asInstanceOf[js.Array[Seq[AnyFrag]]]
        
    gadgetOptsSeq.flatten
  }
  
  @tailrec private def findParentGadgetRec(node:JQuery, pred:AnyFrag => Boolean):AnyFrag = {
    val frags = findGadgets(node)
    frags.find(pred(_)) match {
      case Some(result) => result
      case None => findParentGadgetRec(node.parent(), pred)
    }
  }
  def findParentGadget(pred:AnyFrag => Boolean):AnyFrag = {
    findParentGadgetRec($(elem), pred)
  }
  
  def findGadgets(node:JQuery):Seq[AnyFrag] = {
    if (node.hasClass("_withGadget"))
      node.data("gadgets").asInstanceOf[Seq[AnyFrag]]
    else
      Seq.empty
  }
  
  /**
   * Lazy version of render(). This returns the rendered content of the Gadget, rendering if need
   * be. This allows you to easily fetch the elem repeatedly, without worrying about re-rendering.
   */
  def rendered = {
    if (elemOpt.isEmpty)
      render
    elem
  }
  
  /**
   * The parent of the resulting Node, once it has been created.
   */
  val parentOptRx = Var[Option[dom.Element]](None)
  def parentOpt = parentOptRx()

  /**
   * We intercept applyTo() (which is part of Scalatags), to record the parent of this Node.
   */
  override def applyTo(parent:dom.Element) = {
    parentOptRx() = Some(parent)
    super.applyTo(parent)
  }
}
