package querki.api

/**
 * The base class for "semantic API exceptions" -- that is, well-behaved Exceptions that
 * are considered to be part of the Querki API. The entry points that can throw these should
 * document them, and the Client should handle them properly.
 *
 * The Querki APIs *can* result in other Exceptions. Client code should handle those on a
 * best-effort basis, but generally consider them to be internal errors, at least in the sense
 * that we have failed to properly encapsulate an error from the input.
 *
 * All exceptions will be returned from the API as BadRequest (400) returns, with the exception
 * serialized (in standard uPickle format) as the body of the result.
 */
sealed trait ApiException extends Exception

/**
 * An exception that is allowed to percolate through the API, but which doesn't carry any
 * semantic weight. This should only be used for exceptions that aren't horrible and fatal,
 * but which we don't expect to arise. (Generally cases where we expect the UI to prevent this
 * situation from arising.)
 */
case class MiscException(msg:String) extends ApiException

/**
 * An unknown Thing was requested, and it doesn't make sense to interpret as a Tag.
 */
case class UnknownThingException(thingId:String) extends ApiException


/**
 * Expected exceptions that can be returned from EditFunctions.
 */
sealed trait EditException extends ApiException
case class GeneralChangeFailure(msg:String) extends EditException
/**
 * A value that was sent for saving didn't pass validation for its type.
 *
 * When you hit this exception, it generally suggests that we should be doing stronger validation
 * in the Client. But this serves as a decent belt-and-suspenders check.
 */
case class ValidationException(msg:String) extends EditException
/**
 * Indicates that we hit an inheritance loop. This is propagated to the client
 * so that the Editor can allow you to fix it.
 */
case class ModelLoopException() extends EditException


/**
 * Expected exceptions that can be returned from SecurityFunctions.
 */
sealed trait SecurityException extends ApiException
/**
 * This invitation would exceed the maximum members per Space.
 */
case class MaxMembersPerSpaceException(curMax:Int) extends SecurityException
/**
 * Failed to archive this Space.
 */
case class CanNotArchiveException() extends SecurityException
/**
 * You aren't allowed to do that.
 */
case class NotAllowedException() extends SecurityException {
  // TODO: this is wrong -- we should be setting the message server-side, localized.
  // How can we fill such things in, in the general case, for Exceptions that don't
  // have specific client-side complicity?
  override def toString() = "You aren't allowed to do that"
}
/**
 * You didn't give your password correctly.
 */
case class BadPasswordException() extends SecurityException


/**
 * Expected exceptions that can be returned from AdminFunctions.
 */
sealed trait AdminException extends ApiException
/**
 * You tried to perform an Admin function from a non-Admin account.
 */
case class NotAnAdminException() extends AdminException


/**
 * Expected exceptions from the Import / Export functions.
 */
sealed trait ImportException extends ApiException
/**
 * Tried to create a Space using a name that you already have in use.
 */
case class SpaceExistsException(name:String) extends ImportException


/**
 * Expected exceptions from User/Identity functions.
 */
sealed trait UserException extends ApiException
case class HandleAlreadyTakenException(handle: String) extends UserException
case class EmailAlreadyTakenException(email: String) extends UserException
