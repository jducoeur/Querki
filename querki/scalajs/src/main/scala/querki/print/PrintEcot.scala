package querki.print

import querki.globals._

import querki.data.ThingInfo

class PrintEcot(e:Ecology) extends ClientEcot(e) with Print {
  
  def implements = Set(classOf[Print])
  
  lazy val Client = interface[querki.client.Client]
  lazy val PageManager = interface[querki.display.PageManager]

  def print(thing:ThingInfo) = {
    PageManager.window.print()
  }
}
