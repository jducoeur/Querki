package querki.apps

import akka.actor._

import models.{ThingId}

import querki.data.TID
import querki.globals._
import querki.util.QuerkiActor

import AppsFunctions._

/**
 * The Actor that shepherds the process of extracting a new App from an existing Space. This is separate
 * from the Space's main troupe, so that it can reboot the Space when needed.
 * 
 * Structurally, this is extremely similar to the ImportSpaceActor. It's possible that there might be a
 * common concept to merge from these, of a monitorable Actor that handles a long-running process.
 * 
 * @author jducoeur
 */
private [apps] class ExtractAppActor(e:Ecology, elements:Seq[TID], state:SpaceState) extends QuerkiActor(e) with ExtractAppProgress {
  import ExtractAppActor._
    
  // TEMP:
  QLog.spew("In ExtractAppActor:")
  elements foreach { elementId =>
    state.anything(ThingId(elementId.underlying)) match {
      case Some(element) => QLog.spewThing(element)(state)
      case _ => QLog.error(s"Unknown TID $elementId!")
    }
  }
  
  override def preStart() = {
    self ! Start
  }
  
  def doReceive = {
    case Start => {
      
    }
    
    case GetProgress => {
      // TODO: fill this in for real!
      sender ! ExtractAppProgress("In process", 20, appInfo, failed)
    }
    
    case CompletionAcknowledged => context.stop(self)    
  }
}

object ExtractAppActor {
  def props(e:Ecology, elements:Seq[TID], state:SpaceState) = Props(classOf[ExtractAppActor], e, elements, state) 
  
  case object Start
  
  case object GetProgress
  
  case object CompletionAcknowledged
}
