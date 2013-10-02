package querki.util

import play.api.i18n.Messages
import play.api.mvc.RequestHeader

/**
 * Represents an error that is intended to be displayed to end users. Intentionally forces you to
 * internationalize the message properly. All exceptions that are to be shown to users should use this!
 */
case class PublicException(msgName:String, params:Any*) extends Exception {
  def display(req:RequestHeader) = Messages(msgName, params:_*)
  override def getMessage = "BUG: Trying to display a PublicException without the Request. Use display() instead."
}

/**
 * Represents an internal error that should *not* be shown to end users.
 */
case class InternalException(message:String) extends Exception(message)
