package querki.display

import scalatags.JsDom.all._
import org.scalajs.dom

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
   */
  def render = {
    val result = createFrag
    _elem = Some(result)
    onCreate(result)
    result
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
