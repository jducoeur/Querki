package querki.console

import querki.globals._
import querki.util.PublicException

/**
 * This typeclass represents a Type that can be parsed from the Console.
 */
trait ParamParseable[T] {
  /**
   * Given a string parameter from a console command, parse it or return an error.
   */
  def parse(str:String):Either[PublicException, T]
}

case class ConsoleParam[T : ParamParseable](name:String, desc:String) {
  def parse(str:String):Either[PublicException, T] = {
    implicitly[ParamParseable[T]].parse(str)
  }
}

/**
 * The full signature of a Command that can be issued in the Console, that
 * somebody is going to handle.
 */
abstract class ConsoleCommand(
  val name:String, 
  val desc:String,
  val requiresSpace:Boolean,
  val params:ConsoleParam[_]*
) {
  // TODO: this handler should probably return something more interesting:
  def handle(inv:CommandInvocation):Future[Unit]
}

// TODO: properly speaking, these args should probably be a proper data
// structure that we are destructuring and restructuring using Shapeless,
// so that we don't get so squishy about the types here.
//
// When we have a minute to pause and think, see about redoing the Console
// API like that.

case class FilledArg[T : ParamParseable](name:String, v:T)
case class CommandInvocation(name:String, context:ConsoleContextProvider[_], args:Map[String, FilledArg[_]])
