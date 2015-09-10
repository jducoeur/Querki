package querki

import scala.concurrent.Future

/**
 * @author jducoeur
 */
package object values {
  type QFut = Future[QValue]
}