package querki.email

import scala.concurrent.ExecutionContext
import scala.util.Try

import akka.actor.Scheduler

import models.Wikitext

import querki.ecology._
import querki.globals._
import querki.identity.Identity

/**
 * This gives the full information about a previously-"sent" email. Only used in Test mode.
 */
case class TestEmailMessageDetails(from:String, 
  recipientEmail:EmailAddress, recipientName:String, requester:Identity, 
  subject:Wikitext, bodyMain:Wikitext)
  
/**
 * Represents a single "session" -- a group of emails that were sent together.
 */
trait TestEmailSession {
  def messages:Seq[TestEmailMessageDetails]
}

/**
 * A side-interface, solely for the use of the Functional Test harness, that lets us examine
 * the emails that have been "sent".
 */
trait TestEmailInspector extends EcologyInterface {
  /**
   * Allows the test harness to examine the emails that have been "sent". The most recent session
   * is at the head of the list.
   */
  def sessions:List[TestEmailSession]
}

/**
 * A Test-only implementation of EmailSender, which does nothing but record the emails sent so that
 * they can be inspected by the test harness.
 * 
 * IMPORTANT: this Ecot is stateful! Do not take that as permission to write other stateful Ecots!
 * This is basically evil, but we are allowing it *solely* for functional-test purposes, where the operation
 * is presumed to be predictable and largely non-parallel.
 * 
 * @author jducoeur
 */
private [email] class TestEmailSender(e:Ecology) extends QuerkiEcot(e) with EmailSender with TestEmailInspector { sender =>
  
  /**
   * A test session. IMPORTANT: the messages collection only works because we are synchronizing
   * appending to it! This is necessary because some operations parallelize sending a bunch of
   * emails.
   */
  class TestSession extends EmailSession with TestEmailSession {
    var messages = List.empty[TestEmailMessageDetails]
    def append(details:TestEmailMessageDetails) = {
      sender.synchronized {
        messages = messages :+ details
      }
    }
    override def toString = s"TestSession(${messages.mkString(", ")})"
  }
  type TSession = TestSession

  /**
   * We track the sessions so that the Functional Test harness can look at what's been sent.
   */
  var sessions = List.empty[TSession]
  
  def createSession():TSession = {
    sender.synchronized {
      val session = new TestSession
      sessions = session :: sessions
      session
    }
  }
  
  /**
   * Note: the scheduler below is *null* in the test environment!
   */
  def sendEmail(msg:EmailMsg)(implicit scheduler: Scheduler, ec: ExecutionContext): Future[Int] = {
    val session = createSession()
    val details = TestEmailMessageDetails(msg.from.addr, msg.to, msg.toName, null, msg.subject, msg.body)
    session.append(details)
    Future.successful(250)
  }
  
  def sendInternal(session:TSession, from:String, 
      recipientEmail:EmailAddress, recipientName:String, requester:Identity, 
      subject:Wikitext, bodyMain:Wikitext):Try[Unit] = 
  Try {
    val details = TestEmailMessageDetails(from, recipientEmail, recipientName, requester, subject, bodyMain)
    session.append(details)
  }
}
