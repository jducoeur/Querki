package querki.display

import org.scalajs.dom
import _root_.rx._

/**
 * A controlled wrapper around a Scalatags Frag, which gives you access to the lifecycle and
 * the resulting DOM objects.
 * 
 * IMPORTANT: unlike pure ScalaTags, the Gadgets library assumes that Frags are single-use!
 * That is, in conventional ScalaTags you can create a Frag or TypedTag and render it over
 * and over, getting a new Node each time. By contrast, rendering a ManagedFrag records the
 * resulting Node in its elemOptRx (also exposed as elemOpt or simply elem), and you are
 * encouraged to use the rendered method instead if there is any risk of accessing it
 * multiple times -- the idea is that you create a separate Frag for each bit, and render
 * that once.
 * 
 * Note that a ManagedFrag corresponds to a DOM *Node*, which is almost anything: an Element,
 * an Attribute, a Text, etc. A Gadget (which is a subclass of ManagedFrag) corresponds to
 * a DOM Element.
 * 
 * In and of itself, ManagedFrag is a bit weak, but it is the basis of multiple forms of
 * useful gadgetry.
 */
trait ManagedFrag[Output <: dom.Node] extends scalatags.jsdom.Frag {

  /**
   * An Rx member containing the actual Node iff it has been rendered.
   */
  val elemOptRx = Var[Option[Output]](None)
  def elemOpt = elemOptRx()
  /**
   * Fetches the actual rendered DOM Node for this Frag.
   * 
   * IMPORTANT: this is convenient, but fundamentally unsafe! Only use it in places where
   * you are *certain* that the Node has already been rendered; otherwise, use the safer
   * elemOpt or elemOptRx!
   */
  def elem = elemOpt.get
  
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
