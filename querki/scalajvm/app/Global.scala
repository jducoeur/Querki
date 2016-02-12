import scala.concurrent.duration._

import play.api._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.mvc.WithFilters

import akka.actor._
import akka.util.Timeout

import com.typesafe.config.ConfigFactory
import com.typesafe.conductr.bundlelib.akka.{ Env => AkkaEnv }
import com.typesafe.conductr.bundlelib.play.{ Env => PlayEnv }
import com.typesafe.conductr.bundlelib.play.StatusService
import com.typesafe.conductr.bundlelib.play.ConnectionContext.Implicits.defaultContext

import controllers.LoggingFilter

import models._

import querki.ecology.Ecology
import querki.system.QuerkiRoot
import querki.system.QuerkiRoot._

object Global extends WithFilters(LoggingFilter) with GlobalSettings {
  
  var _appSystem:ActorSystem = null
  var _root:ActorRef = null
  
  var ecology:Ecology = null
  
  val initTermDuration = 30 seconds
  implicit val initTermTimeout = Timeout(initTermDuration)
  
  // Incorporate ConductR's configuration:
  val totalConfiguration = super.configuration ++ Configuration(PlayEnv.asConfig)
  override def configuration: Configuration = totalConfiguration
  
  override def onStart(app: Application) {
    // Prep ConductR, if it's present:
    val config = AkkaEnv.asConfig
    // I suspect this fallback shouldn't be "application", but if I set to it anything else I
    // get errors. It really feels like there are internals that are looking for "application".
    val systemName = sys.env.getOrElse("BUNDLE_SYSTEM", "application")
    _appSystem = ActorSystem(systemName, config.withFallback(ConfigFactory.load()))
    
    // Tell the QuerkiRoot to initialize and wait for it to be ready. Yes, this is one of those
    // very rare times when we really and for true want to block, because we don't want to consider
    // ourselves truly started until it's done:
    _root = _appSystem.actorOf(Props[QuerkiRoot], "querkiRoot")
    val fut = akka.pattern.ask(_root, QuerkiRoot.Initialize)
    val result = scala.concurrent.Await.result(fut, initTermDuration)
    result match {
      case Initialized(e) => ecology = e
      case _ => Logger.error("Got an unexpected result from QuerkiRoot.Initialize!!!")
    }
    
    // Evil workaround, to give the functional test harness access to the running Ecology:
    QuerkiRoot.ecology = ecology
    
    Logger.info("Querki has started")
    
    // Tell ConductR to open the door:
    StatusService.signalStartedOrExit()
  }  
  
  override def onStop(app: Application) {
    Logger.info("Querki shutting down...")
    
    val fut = akka.pattern.ask(_root, QuerkiRoot.Terminate)
    scala.concurrent.Await.result(fut, initTermDuration)
    
    _appSystem.shutdown()
    
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
