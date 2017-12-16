package querki.console

import querki.globals._

class ConsoleEcot(e:Ecology) extends ClientEcot(e) with Console {
  def implements = Set(classOf[Console])
  
  lazy val Pages = interface[querki.pages.Pages]

  lazy val consoleFactory = Pages.registerStandardFactory("_console", { new ConsolePage(_) }) 
  
  override def postInit() = {
    consoleFactory
  }
}
