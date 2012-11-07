import play.api._

import models._

object Global extends GlobalSettings {
  
  override def onStart(app: Application) {
    
    SpaceManager.ask[String,Unit](SaySomething("Newly woken up")) { Logger.info(_) }
    
    Logger.info("Querki has started")
  }  
  
  override def onStop(app: Application) {
    Logger.info("Querki shutting down...")
  }  
  
}