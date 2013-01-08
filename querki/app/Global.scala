import play.api._

import models._

object Global extends GlobalSettings {
  
  override def onStart(app: Application) {
    Logger.info("Initializing System Space...")
    system.SystemSpace.init
    Logger.info("Querki has started")
  }  
  
  override def onStop(app: Application) {
    Logger.info("Querki shutting down...")
    system.SystemSpace.term
  }  
  
}