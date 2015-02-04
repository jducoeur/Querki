package querki.api

sealed trait ApiException extends Exception

sealed trait EditException extends ApiException

case class GeneralChangeFailure() extends EditException
