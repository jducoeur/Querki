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
