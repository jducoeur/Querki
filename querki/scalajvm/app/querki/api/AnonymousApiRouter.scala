package querki.api

import akka.actor._
import akka.event.LoggingReceive

import org.querki.requester._

import querki.globals._
import querki.values.RequestContext

/**
 * This Actor is intended as a handler for API calls that are fundamentally anonymous -- that
 * hook into neither a User nor a Space, and which do not require a logged-in User. It spawns
 * a worker for each API request, purely locally. There is one of these per node.
 * 
 * TODO: ideally, this should have a pool of children handling the API calls.
 * 
 * @author jducoeur
 */
class AnonymousApiRouter(val ecology:Ecology) extends Actor {
  def receive = {
    case msg:ClientRequest => {
      val worker = context.actorOf(Props(classOf[AnonymousApiWorker], ecology))
      worker.forward(msg)
    }
  }
}

object AnonymousApiRouter {
  def actorProps(ecology:Ecology) = Props(classOf[AnonymousApiRouter], ecology)
}

/**
 * We create one of these for each Anonymous API invocation. This Actor self-destructs when complete.
 */
private [api] class AnonymousApiWorker(val ecology:Ecology) extends Actor with EcologyMember with Requester {
  lazy val ApiInvocation = interface[querki.api.ApiInvocation]

  def mkParams(rc:RequestContext) = AutowireParams(rc.requesterOrAnon, None, rc, None, this, sender)
  
  def receive = LoggingReceive (handleRequestResponse orElse {
    case ClientRequest(req, rc) => {
      // Handle this request, then shut down when finished:
      ApiInvocation.handleSessionRequest(req, mkParams(rc), { dummy => context.stop(self) })
    }
  })
}
