import play.api._

import scala.concurrent.duration._

import akka.actor.Props
import akka.util.Timeout

import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.mvc.WithFilters

import controllers.LoggingFilter

import models._

import querki.ecology.Ecology
import querki.system.QuerkiRoot
import querki.system.QuerkiRoot._

object Global extends WithFilters(LoggingFilter) with GlobalSettings {
  
  lazy val root = Akka.system.actorOf(Props[QuerkiRoot], "querkiRoot")
  
  var ecology:Ecology = null
  
  val initTermDuration = 30 seconds
  implicit val initTermTimeout = Timeout(initTermDuration)
  
  override def onStart(app: Application) {
    // Tell the QuerkiRoot to initialize and wait for it to be ready. Yes, this is one of those
    // very rare times when we really and for true want to block, because we don't want to consider
    // ourselves truly started until it's done:
    val fut = akka.pattern.ask(root, QuerkiRoot.Initialize)
    val result = scala.concurrent.Await.result(fut, initTermDuration)
    result match {
      case Initialized(e) => ecology = e
      case _ => Logger.error("Got an unexpected result from QuerkiRoot.Initialize!!!")
    }
    
    Logger.info("Querki has started")
  }  
  
  override def onStop(app: Application) {
    Logger.info("Querki shutting down...")
    
    val fut = akka.pattern.ask(root, QuerkiRoot.Terminate)
    scala.concurrent.Await.result(fut, initTermDuration)
    
    Logger.info("... Done")
  }
  
  /**
   * Getter for Controllers, which injects the Ecology into them.
   * 
   * IMPORTANT: the consequence of this is that all controller routes need to start with '@',
   * which tells Play to use this mechanism. It also implies that all of our controllers need
   * to be classes, not objects. They should all derive from ApplicationBase.
   */
  override def getControllerInstance[A](controllerClass: Class[A]) : A = {
    val controller = super.getControllerInstance(controllerClass)
    controller match {
      case querkiController:controllers.ApplicationBase => {
        // Inject the Ecology into the controller:
        querkiController.ecology = ecology
      }
      case _ => { }
    }
    controller
  }  
}
