package querki.display.rx

import org.scalajs.dom.html.Element
import scalatags.JsDom.all._
import rx._
import org.querki.gadgets._
import org.querki.jquery._

import querki.globals._

class QGadgetRef[G <: Gadget[_]](implicit val ecology:Ecology) extends GadgetRef[G] with EcologyMember {
  // TODO: this interface, and the EcologyMember trait in general, is solely here to
  // support Pages.updatePage(). We need to make this more generally hookable, so we
  // can extract GadgetRef up to the library.
  lazy val Pages = interface[querki.pages.Pages]
  
  override def doReassign(g:G, retainPrevious:Boolean) = {
    super.doReassign(g, retainPrevious)

    // Since we're changing the page layout, update things:
    Pages.updatePage(this)
  }
}

/**
 * A variant of GadgetRef, which you should use when you're just dealing with a raw TypedTag, not
 * a Gadget per se.
 * 
 * All this really does is provide type-safety for this situation, without requiring that GadgetRef
 * itself know about T.
 * 
 * Create this using GadgetRef.of[].
 */
class QGadgetElementRef[T <: Element](implicit e:Ecology) extends QGadgetRef[Gadget[T]] {
  /**
   * This is similar to GadgetRef <= operation, but works with a raw TypedTag and wraps it in a
   * Gadget. This is used when you declared it with GadgetRef.of[].
   */
  def <=(tag:scalatags.JsDom.TypedTag[T]) = reassign(new TypedGadget(tag), false)
  def <~(tag:scalatags.JsDom.TypedTag[T]) = reassign(new TypedGadget(tag), true)
}

object QGadgetRef {
  /**
   * The usual way to create a GadgetRef for a particular type of Gadget.
   */
  def apply[G <: Gadget[_]](implicit e:Ecology) = new QGadgetRef[G]
  /**
   * This allows you to create a GadgetRef that will work with an underlying TypedTag.
   */
  def of[T <: Element](implicit e:Ecology) = new QGadgetElementRef[T]
}
