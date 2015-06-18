package querki.display

import org.querki.jquery._

/**
 * @author jducoeur
 */
package object rx {
  /**
   * This is the evidence required to use an Rx as an AttrValue in Scalatags.
   */
  implicit def rxAttr[T <% AttrVal] = new RxAttr[T]
  implicit def varAttr[T <% AttrVal] = new VarAttr[T]
}