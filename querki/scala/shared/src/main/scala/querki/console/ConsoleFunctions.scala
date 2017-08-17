package querki.console

import scala.concurrent.Future

/**
 * This is the API for the Querki Console.
 */
trait ConsoleFunctions {
  def consoleCommand(cmd:String):Future[Unit]
}
