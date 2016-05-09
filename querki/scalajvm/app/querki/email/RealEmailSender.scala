package querki.email

import scala.util.Try

import javax.activation.DataHandler
import javax.mail._
import javax.mail.internet._
import javax.mail.util.ByteArrayDataSource
import com.sun.mail.smtp._

import models.Wikitext

import querki.ecology._
import querki.globals._
import querki.identity.Identity

/**
 * The real implementation of the EmailSender interface, which is used everywhere *except* in testing.
 * 
 * @author jducoeur
 */
private [email] class RealEmailSender(e:Ecology) extends QuerkiEcot(e) with EmailSender {
  
  case class SessionWrapper(session:Session) extends EmailSession
  type TSession = SessionWrapper
  
  def fullKey(key:String) = "querki.mail." + key
  lazy val from = Config.getString(fullKey("from"))
  lazy val smtpHost = Config.getString(fullKey("smtpHost"))
  lazy val smtpPort = Config.getInt(fullKey("port"), 0)
  lazy val debug = Config.getBoolean(fullKey("debug"), false)
  lazy val username = Config.getString(fullKey("smtpUsername"), "")
  lazy val password = Config.getString(fullKey("smtpPassword"), "")
  
  def createSession():TSession = {
    val props = System.getProperties()
    props.setProperty("mail.host", smtpHost)
    if (smtpPort != 0) {
      props.setProperty("mail.smtp.port", smtpPort.toString)
    }
    props.setProperty("mail.user", "querki")
    props.setProperty("mail.transport.protocol", "smtp")
    props.setProperty("mail.from", from)
    props.setProperty("mail.debug", "true")
    if (username.length() > 0) {
      // We're opening an authenticated session
      props.setProperty("mail.smtp.auth", "true")
      props.setProperty("mail.smtp.starttls.enable", "true")
      props.setProperty("mail.smtp.starttls.required", "true")
    }
      
    val session = Session.getInstance(props, null)
  
    session.setDebug(debug)
    
    SessionWrapper(session)
  }
  
  /**
   * The guts of actually sending email. Note that this can be invoked from a number of different circumstances,
   * some user-initiated and some not.
   */
  def sendInternal(sessionWrapper:TSession, from:String, 
      recipientEmail:EmailAddress, recipientName:String, requester:Identity, 
      subject:Wikitext, bodyMain:Wikitext):Try[Unit] = 
  Try {
    val SessionWrapper(session) = sessionWrapper
    val msg = new MimeMessage(session)
    msg.setFrom(new InternetAddress(from))
  
    val replyAddrs = Array(new InternetAddress(requester.email.addr, requester.name).asInstanceOf[Address])
    msg.setReplyTo(replyAddrs)
  
      // TBD: there must be a better way to do this. It's a nasty workaround for the fact
    // that setRecipients() is invariant, I believe.
    val toAddrs = Array(new InternetAddress(recipientEmail.addr, recipientName).asInstanceOf[Address])  
    msg.setRecipients(Message.RecipientType.TO, toAddrs)
    
    msg.setSubject(subject.plaintext)
    
    val body = bodyMain + Wikitext("""
      |
      |------
      |
      |If you believe you have received this message in error, please drop a note to "betaemail@querki.net". (We're
      |working on the one-click unsubscribe, but not quite there yet; sorry.)""".stripMargin)
    
    // Attach the HTML...
    val bodyHtml = body.display
    val bodyPartHtml = new MimeBodyPart()
    bodyPartHtml.setDataHandler(new DataHandler(new ByteArrayDataSource(bodyHtml, "text/html")))      
    // ... and the plaintext...
    val bodyPlain = body.plaintext
    val bodyPartPlain = new MimeBodyPart()
    bodyPartPlain.setDataHandler(new DataHandler(new ByteArrayDataSource(bodyPlain, "text/plain")))
    // ... and set the body to the multipart:
    val multipart = new MimeMultipart("alternative")
    // IMPORTANT: these are in increasing order of importance, and Gmail will display the *last*
    // one by preference:
    multipart.addBodyPart(bodyPartPlain)
    multipart.addBodyPart(bodyPartHtml)
    msg.setContent(multipart)
        
    msg.setHeader("X-Mailer", "Querki")
    msg.setSentDate(new java.util.Date())
        
    if (username.length > 0) {
      val transport = session.getTransport("smtp").asInstanceOf[SMTPTransport]
      transport.connect(username, password)
      transport.sendMessage(msg, msg.getAllRecipients)
      if (debug)
        QLog.spew(s"Sent email; transport returned ${transport.getLastServerResponse}")
    } else {
      // Non-TLS -- running on a test server, so just do it the easy way:
      Transport.send(msg)
    }
  }

}