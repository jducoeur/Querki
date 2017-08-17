package querki.console

import querki.api.{SpaceApiImpl, AutowireParams}
import querki.globals._

class ConsoleFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with ConsoleFunctions {
  
  lazy val Console = interface[querki.console.Console]
  
  def doRoute(req:Request):Future[String] = route[ConsoleFunctions](this)(req)
  
  def consoleCommand(cmd:String):Future[Unit] = {
    // TODO: Why isn't the second parameter here working implicitly? I'm missing something about
    // the typeclass invocation.
    Console.invoke(cmd)(ConsoleContextProvider.stateContextProvider(this))
  }
}
