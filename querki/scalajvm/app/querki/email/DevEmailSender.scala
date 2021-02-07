package querki.email

import akka.actor.Scheduler
import models.Wikitext
import querki.globals.{QuerkiEcot, Ecology}
import querki.identity.Identity

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success}

/**
 * Dev-only variant of the email session.
 *
 * This is the "I give up" variant, for times when everyone is making it too hard for me to find a
 * SendMail server. It just prints out the email.
 *
 * Obviously, this doesn't suffice for testing the real email stack. But it is adequate for testing things
 * like invitations *without* needing to send real emails.
 */
private [email] class DevEmailSender(e:Ecology) extends QuerkiEcot(e) with EmailSender {
  case class DevSession() extends EmailSession
  val theSession = DevSession()
  type TSession = DevSession

  def createSession():TSession = theSession

  def sendInternal(session:TSession, from:String,
    recipientEmail:EmailAddress, recipientName:String, requester:Identity,
    subject:Wikitext, bodyMain:Wikitext): Try[Unit] =
  {
    val printout =
      s"""Email would be sent:
         |from $from
         |to $recipientName (${recipientEmail.addr})
         |
         |Subject: ${subject.display.str}
         |
         |${bodyMain.display.str}""".stripMargin
    println(printout)
    Success(())
  }

  def sendEmail(msg:EmailMsg)(implicit scheduler: Scheduler, ex: ExecutionContext): Future[Int] = {
    sendInternal(
      theSession,
      msg.from.addr,
      msg.to,
      msg.toName,
      Identity.AnonymousIdentity,
      msg.subject,
      msg.body
    )
    Future.successful(1)
  }
}
