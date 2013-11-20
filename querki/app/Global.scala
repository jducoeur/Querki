import play.api._

import akka.actor.Props

import play.api.Play.current
import play.api.libs.concurrent.Akka

import models._

import querki.system.QuerkiRoot

object Global extends GlobalSettings {
  
  override def onStart(app: Application) {
    val root = Akka.system.actorOf(Props[QuerkiRoot], "querkiRoot")
    
    // Tell the QuerkiRoot to initialize and wait for it to be ready. Yes, this is one of those
    // very rare times when we really and for true want to block, because we don't want to consider
    // ourselves truly started until it's done:
    val fut = akka.pattern.ask(root, QuerkiRoot.Initialize)(akka.util.Timeout(30000))
    scala.concurrent.Await.result(fut, scala.concurrent.duration.Duration("30 seconds"))
    
    Logger.info("Querki has started")
  }  
  
  override def onStop(app: Application) {
    Logger.info("Querki shutting down...")
    system.SystemSpace.term
  }  
  
}