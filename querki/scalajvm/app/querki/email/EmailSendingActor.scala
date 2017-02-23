package querki.email

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
  
  lazy val EmailSender = interface[EmailSender]
  
  def doReceive:Receive = {
    case msg:EmailMsg => 
      try {
        EmailSender.sendEmail(msg)
      } catch {
        case ex:Exception => QLog.error(s"Exception while sending email $msg", ex)
      }
  }
}
