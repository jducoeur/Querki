package querki.history

import scala.concurrent.duration._
import akka.util.Timeout

import querki.globals._

class HistoryCommands(e: Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  val Console = initRequires[querki.console.Console]
  val History = initRequires[History]

  implicit val timeout = Timeout(30.seconds)

  override lazy val props = Seq(
  )
}
