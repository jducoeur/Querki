package querki

import models.Property
import querki.globals._
import querki.ql.Invocation
import querki.values.QValue

package object console {
  import ConsoleFunctions._

  trait Console extends EcologyInterface {
    
    def SpaceCommandProp:Property[OID, OID]
    def AdminCommandProp:Property[Boolean, Boolean]
    
    /**
     * Invoke a command. This will be parsed and executed.
     * 
     * TODO: this will want to return something more interesting than Unit.
     */
    def invoke[T : ConsoleContextProvider](cmd:String):Future[CommandResult]
    
    /**
     * Convenience definition for defining standard Admin commands. Note that you still have
     * to have Console as an init dependency to use this, though.
     */
    def defineAdminCommand(oid:OID, name:String, summary:String, otherProps:(OID, QValue)*)
      (handler:Invocation => Future[CommandResult]):Property[String, String]
  }
}
