package querki.display

import scala.scalajs.js
import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.jquery._

import querki.globals._

/**
 * A controlled wrapper around a Scalatags Frag, which gives you access to the lifecycle and
 * the resulting DOM objects.
 * 
 * In and of itself, ManagedFrag is a bit weak, but it is the basis of multiple forms of
 * useful gadgetry.
 */
trait ManagedFrag[Output <: dom.Node] extends scalatags.jsdom.Frag {
  
  var _elem:Option[Output] = None
  def elemOpt = _elem
  def elem = _elem.get
  
  /**
   * Slam the element for this Gadget. You should only call this iff the element was actually called from
   * an external mechanism (eg, via QText), and you're building this Gadget around that element.
   * 
   * This is intentionally designed for chaining, for ease of use -- it returns this Gadget.
   */
  def setElem(e:dom.Node):this.type = {
    _elem = Some(e.asInstanceOf[Output])
    $(elem).data("gadget", this.asInstanceOf[js.Any])
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
    result
  }
  
  type AnyNode <: dom.Node
  type AnyFrag = ManagedFrag[AnyNode]
  
  def findGadgetsFor(root:JQuery, pred:AnyFrag => Boolean):Seq[AnyFrag] = {
    val gadgetOptsArray = root.find("._withGadget").map({ (e:dom.Element) =>
      val frag = $(e).data("gadget").asInstanceOf[AnyFrag]
      println(s"Found element $e with frag $frag")
      if (pred(frag))
        Some(frag)
      else
        None
    }:js.ThisFunction0[dom.Element, Any]).jqf.get()
    
    val gadgetOptsSeq:Seq[Option[AnyFrag]] = gadgetOptsArray.asInstanceOf[js.Array[Option[AnyFrag]]]
        
    gadgetOptsSeq.flatten
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
  var parentOpt:Option[dom.Element] = None

  /**
   * We intercept applyTo() (which is part of Scalatags), to record the parent of this Node.
   */
  override def applyTo(parent:dom.Element) = {
    parentOpt = Some(parent)
    super.applyTo(parent)
  }
}
