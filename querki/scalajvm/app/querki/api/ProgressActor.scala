package querki.api

import scala.concurrent.Future

import akka.actor._

import querki.globals._

/**
 * ProgressActor is an Actor mix-in, which encapsulates the concept of a process that can be
 * monitored by the Client. It is specifically a worker.
 * 
 * Concrete Actors should mix this in, and *must* call handleProgress as one clause of their
 * receive function.
 * 
 * @author jducoeur
 */
trait ProgressActor { asActor:Actor =>
  import ProgressActor._
  
  /**
   * The concrete Actor must fill this in with a description of what's happening, and update
   * it as appropriate.
   */
  var phaseDescription:String = ""
  
  /**
   * The concrete Actor must define this, to return the current percent complete. It should
   * return 100 when it is completely finished and ready to shut down.
   */
  def calcProgress():Int
  
  /**
   * The concrete Actor should switch this to true iff the process has failed.
   */
  var failed = false
  
  def failWith(msg:String) = {
    failed = true
    QLog.error(msg)
    throw new Exception(msg)
  }
  
  /**
   * The concrete Actor must define this. It does the actual work. This will typically involve
   * lots of Requests.
   */
  def doWork():Unit
  
  /**
   * The main receive function for ProgressActor. If the concrete Actor overrides receive, it must
   * call this manually.
   */
  def handleProgress:Receive = {
    case Start => doWork()
    
    case GetProgress => {
      val percent = calcProgress()
      sender ! OperationProgress(phaseDescription, percent, (failed || percent >= 100), failed)
    }
    
    case CompletionAcknowledged => context.stop(context.self)
  }
  
  def receive = handleProgress
}

object ProgressActor {
  
  case object Start
  case object GetProgress
  case object CompletionAcknowledged
  
  /**
   * Create a long-running child Actor, and return an OperationHandle pointing to it.
   */
  def createProgressActor(requester:Actor, props:Props):OperationHandle = {
    val importActor = requester.context.actorOf(props)
    
    importActor ! Start
    
    // Now, return the fully-qualified path to that Actor:
    val path = importActor.path
    val system = requester.context.system
    val defaultAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
    ActorOperationHandle(path.toStringWithAddress(defaultAddress))    
  }
}