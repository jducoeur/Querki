package querki

import querki.globals._

package object console {
  import ConsoleFunctions._

  trait Console extends EcologyInterface {
    /**
     * Invoke a command. This will be parsed and executed.
     * 
     * TODO: this will want to return something more interesting than Unit.
     */
    def invoke[T : ConsoleContextProvider](cmd:String):Future[CommandResult]
  }
}
