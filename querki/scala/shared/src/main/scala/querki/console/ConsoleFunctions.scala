package querki.console

import scala.concurrent.Future

/**
 * This is the API for the Querki Console.
 */
trait ConsoleFunctions {
  import ConsoleFunctions._
  def consoleCommand(cmd: String): Future[CommandResult]
}

object ConsoleFunctions {
  sealed trait CommandResult
  case class DisplayTextResult(str: String) extends CommandResult
  case class ErrorResult(str: String) extends CommandResult
}
