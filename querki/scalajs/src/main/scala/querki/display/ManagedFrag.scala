package querki.display

import scala.annotation.tailrec

import scala.scalajs.js
import js.UndefOr
import org.scalajs.dom
import org.querki.jquery._
import _root_.rx._

/**
 * A controlled wrapper around a Scalatags Frag, which gives you access to the lifecycle and
 * the resulting DOM objects.
 * 
 * Note that a ManagedFrag corresponds to a DOM *Node*, which is almost anything: an Element,
 * an Attribute, a Text, etc. A Gadget (which is a subclass of ManagedFrag) corresponds to
 * a DOM Element.
 * 
 * In and of itself, ManagedFrag is a bit weak, but it is the basis of multiple forms of
 * useful gadgetry.
 */
trait ManagedFrag[Output <: dom.Node] extends scalatags.jsdom.Frag {
  
  val elemOptRx = Var[Option[Output]](None)
  def elemOpt = elemOptRx()
  def elem = elemOpt.get
  
  /**
   * Fetch a JQuery for the underlying Element of this Ref.
   * 
   * IMPORTANT: this will throw an Exception if the Ref isn't assigned, or the Gadget isn't rendered yet!
   */
  def jq = $(elem)
  
  /**
   * Slam the element for this Gadget. You should only call this iff the element was created from
   * an external mechanism and you're building this Gadget around that element.
   * 
   * This is intentionally designed for chaining, for ease of use -- it returns this Gadget.
   */
  def setElem(e:Output):this.type = {
    elemOptRx() = Some(e)
    this
  }
  
  /**
   * Concrete classes must define this. It causes the actual DOM node to come into existence.
   */
  def createFrag:Output
  
  /**
   * Subclasses can override this to define behaviour that happens in the middle of the render
   * function. Generally used by infrastructure, not concrete classes.
   */
  def onRendered(node:Output):Unit
  
  /**
   * This is called immediately after this fragment is inserted into its parent, so that you can
   * provide additional stuff that happens then.
   */
  def onInserted():Unit = {}
  
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
    onRendered(result)
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
  
  @tailrec private def findParentGadgetRec(node:JQuery, pred:AnyFrag => Boolean):Option[AnyFrag] = {
    if (node.length == 0)
      // Implication is that we've gone all the way to the top of the hierarchy without a match, so
      // give up:
      None
    else {
      val frags = findGadgets(node)
      frags.find(pred(_)) match {
        case Some(result) => Some(result)
        case None => {
          val parent = node.parent()
          if (parent.length > 0 && parent.get(0).get == dom.document)
            None
          else
            findParentGadgetRec(node.parent(), pred)
        }
      }
    }
  }
  def findParentGadget(pred:AnyFrag => Boolean):Option[AnyFrag] = {
    elemOpt.flatMap(e => findParentGadgetRec($(e), pred))
  }
  
  def findGadgets(node:JQuery):Seq[AnyFrag] = {
    if (node.hasClass("_withGadget"))
      node.data("gadgets").map(_.asInstanceOf[Seq[AnyFrag]]).getOrElse(Seq.empty)
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
    onInserted()
  }
}
