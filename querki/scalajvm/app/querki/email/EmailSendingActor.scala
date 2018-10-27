package querki.email

import scala.collection.immutable.Queue
import scala.concurrent.duration._

import akka.actor._

import querki.globals._
import querki.util.QuerkiActor

/**
 * This trivial Actor actually sends out emails. For the moment it is a node singleton, to provide us with a rough
 * throttle of no more than one outgoing email per node at a time.
 * 
 * Eventually, it should get smarter, probably with a pool of senders. But keeping sending controlled is probably
 * a good idea.
 * 
 * Note that this delegates all the real work to the stateless EmailSender. During functional tests, this gets
 * swapped out for a test version. All this Actor does is mediate the threading.
 */
class EmailSendingActor(e:Ecology) extends QuerkiActor(e) {
  import EmailSendingActor._
  
  lazy val EmailSender = interface[EmailSender]
  
  lazy val throttle = Config.getDuration("querki.mail.throttle", 1 second)
  
  private var emailQueue: Queue[EmailEvent] = Queue.empty
  
  override def preStart(): Unit = {
    // Start up a regular event that will send emails on a throttle:
    context.system.scheduler.schedule(throttle, throttle, self, DoSend)
  }
  
  def doReceive:Receive = {
    case msg:EmailMsg => emailQueue = emailQueue.enqueue(EmailEvent(sender, msg))
      
    // This gets called once per tick:
    case DoSend => {
      emailQueue.dequeueOption match {
        case Some((EmailEvent(snd, msg), newQueue)) => {
          emailQueue = newQueue
          try {
            // TBD: we might want to map the result of this -- a Future -- and send the response
            // to the original caller. (Usually the sendEmail() function in EmailModule.)
            // But this may be tens or hundreds of seconds after the message was originally dispatched,
            // so it isn't obviously useful. We might possibly want to notify the sending Space about
            // failures, though.
            EmailSender.sendEmail(msg)(context.system.scheduler, context.dispatcher)
          } catch {
            case ex:Exception => QLog.error(s"Exception while sending email $msg", ex)
          }          
        }
        
        case None => // Nothing to do this tick
      }
    }
  }
}

object EmailSendingActor {
  case class EmailEvent(sender: ActorRef, msg: EmailMsg)
  
  /**
   * Every tick, the EmailSendingActor receives this event, and sends out one email if there is one. This
   * allows us to throttle email sends in an Akkish way.
   */
  private case object DoSend
}
