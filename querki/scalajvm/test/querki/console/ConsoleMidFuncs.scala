package querki.console

import autowire._

import querki.globals._
import querki.test.mid._

import AllFuncs._

import ConsoleFunctions._

trait ConsoleMidFuncs {

  def consoleCommand(cmd: String): TestOp[CommandResult] =
    TestOp.client { _[ConsoleFunctions].consoleCommand(cmd).call() }

  //////////

  def consoleCommandWithTextResult(cmd: String): TestOp[String] = {
    consoleCommand(cmd).map { result =>
      result match {
        case DisplayTextResult(text) => text
        case _                       => throw new Exception(s"Got unexpected result $result from command $cmd")
      }
    }
  }
}

object ConsoleMidFuncs extends ConsoleMidFuncs
