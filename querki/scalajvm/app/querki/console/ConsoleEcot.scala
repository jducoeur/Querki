package querki.console

import querki.ecology._
import querki.globals._

class ConsoleEcot(e:Ecology) extends QuerkiEcot(e) with Console {
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  var commands = Map.empty[String, ConsoleCommand]
  
  def registerCommand(cmd:ConsoleCommand):Unit = commands += (cmd.name -> cmd)
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[ConsoleFunctions, ConsoleFunctionsImpl](SpaceOps.spaceRegion, true, true)
  }
  
  def invoke[T : ConsoleContextProvider](cmdStr:String):Future[Unit] = {
    val context = implicitly[ConsoleContextProvider[T]]
    
    val pieces = cmdStr.split(" ")
    // TODO: these non-local throws are horrible, but Either isn't biased yet in 2.11. Fix this
    // using something adequately biased:
    if (pieces.length == 0) {
      throw new PublicException("Console.error", s"That isn't a legal command")
    }
    val cmd = commands.get(pieces(0)).getOrElse(throw new PublicException("Console.error", s"There is no command named ${pieces(0)}"))
    
    // Okay, we now have the ConsoleCommand.
    // TODO: parse parameters!
    
    val inv = CommandInvocation(cmd.name, context, Map.empty)
    cmd.handle(inv)
  }
}
