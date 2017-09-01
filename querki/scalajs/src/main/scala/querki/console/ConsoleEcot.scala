package querki.console

import querki.globals._

class ConsoleEcot(e:Ecology) extends ClientEcot(e) {
  def implements = Set.empty
  
  lazy val Pages = interface[querki.pages.Pages]

  lazy val consoleFactory = Pages.registerStandardFactory("_console", { new ConsolePage(_) }) 
  
  override def postInit() = {
    consoleFactory
  }
}
