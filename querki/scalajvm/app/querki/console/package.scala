package querki

import querki.globals._

package object console {
  trait Console extends EcologyInterface {
    /**
     * Registers a command that can be handled in the Console.
     * 
     * This should usually be called during postInit().
     */
    def registerCommand(cmd:ConsoleCommand):Unit
    
    /**
     * Invoke a command. This will be parsed and executed.
     * 
     * TODO: this will want to return something more interesting than Unit.
     */
    def invoke[T : ConsoleContextProvider](cmd:String):Future[Unit]
  }
}
