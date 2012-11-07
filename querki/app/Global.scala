import play.api._
import play.api.libs.concurrent._
import play.api.Play.current

import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout

import models._

object Global extends GlobalSettings {
  
  implicit val timeout = Timeout(5 seconds)
  
  override def onStart(app: Application) {
    
    SpaceManager.ask[String,Unit](SaySomething("Newly woken up")) { Logger.info(_) }
    
    Logger.info("Querki has started")
  }  
  
  override def onStop(app: Application) {
    Logger.info("Querki shutting down...")
  }  
  
}