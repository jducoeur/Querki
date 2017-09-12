package querki.console

import querki.api.{SpaceApiImpl, AutowireParams}
import querki.globals._
import querki.util.PublicException

import ConsoleFunctions._

class ConsoleFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with ConsoleFunctions {
  
  lazy val Console = interface[querki.console.Console]
  
  def doRoute(req:Request):Future[String] = route[ConsoleFunctions](this)(req)
  
  def consoleCommand(cmd:String):Future[CommandResult] = {
    Console.invoke(this, cmd)
      .recover {
        case ConsoleException(msg) => ErrorResult(msg)
        case ex:PublicException => ErrorResult(ex.display(Some(rc)))
      }
  }
}
