package querki.email

import scala.concurrent.{ExecutionContext, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

import javax.activation.DataHandler
import javax.mail._
import javax.mail.internet._
import javax.mail.util.ByteArrayDataSource
import com.sun.mail.smtp._

import akka.actor.Scheduler
import akka.pattern.after

import models.Wikitext

import querki.ecology._
import querki.globals._
import querki.identity.Identity

/**
 * The real implementation of the EmailSender interface, which is used everywhere *except* in testing.
 *
 * @author jducoeur
 */
private[email] class RealEmailSender(e: Ecology) extends QuerkiEcot(e) with EmailSender {

  case class SessionWrapper(session: Session) extends EmailSession
  type TSession = SessionWrapper

  def fullKey(key: String) = "querki.mail." + key
  lazy val from = Config.getString(fullKey("from"))
  lazy val smtpHost = Config.getString(fullKey("smtpHost"))
  lazy val smtpPort = Config.getInt(fullKey("port"), 0)
  lazy val debug = Config.getBoolean(fullKey("debug"), false)
  lazy val username = Config.getString(fullKey("smtpUsername"), "")
  lazy val password = Config.getString(fullKey("smtpPassword"), "")

  def createSession(): TSession = {
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
      // AWS now requires TLS 1.2
      props.setProperty("mail.smtp.ssl.protocols", "TLSv1.2")
      props.setProperty("mail.smtp.starttls.required", "true")
    }

    val session = Session.getInstance(props, null)

    session.setDebug(debug)

    SessionWrapper(session)
  }

  /**
   * This is the stylesheet used for Querki's emails. In theory it would be nice to have this in the
   * database itself, to make it more easily modifiable, but for now this is a start.
   */
  val emailStylesheet = """<style>
                          |.para {
                          |  margin-bottom: 7px;
                          |}
                          |
                          |.title {
                          |  font-weight: bold;
                          |  font-size: 16px;
                          |}
                          |
                          |.bottomlinkdiv {
                          |  margin-top: 7px;
                          |  margin-bottom: 7px;
                          |  text-align: center;
                          |  color: #0A9826;
                          |  font-weight: bold;
                          |  font-size: 20px;
                          |}
                          |
                          |.logocontainer {
                          |  text-align: center;
                          |}
                          |
                          |.logo {
                          |  width: 97px;
                          |}
                          |
                          |hr {
                          |  border-top: 1px solid #0A9826;
                          |  height: 1px;
                          |  padding: 0;
                          |  opacity: 0.5;
                          |}
                          |
                          |.maindiv {
                          |  border: 2px solid;
                          |  border-color: #0A9826;
                          |  border-radius: 25px;
                          |  padding: 10px 20px 20px 20px;
                          |}
                          |
                          |.footer {
                          |  margin: 8px 50px 0px 50px;
                          |  font-size: 11px;
                          |}
                          |
                          |.footerAddr {
                          |  margin: 6px 50px 0px 50px;
                          |  font-size: 8px;
                          |}
                          |</style>""".stripMargin

  val retryDelay = 1 second

  def futureWithRetries[R](
    name: String
  )(
    f: => R,
    retries: Int = 3
  )(implicit
    scheduler: Scheduler,
    ex: ExecutionContext
  ): Future[R] = {
    val future = Future { f }
    if (retries > 0) {
      future.recoverWith {
        case ex: Exception => {
          if (debug) QLog.spew(s"$name() got Exception ${ex.getMessage}; retrying $retries")
          // We delay a little, then try again:
          after(retryDelay, scheduler) { futureWithRetries(name)(f, retries - 1) }
        }
      }
    } else {
      // We've run out of retries, so let failures be failures:
      future.onComplete {
        _ match {
          case Success(_)  => // Yay!
          case Failure(ex) => QLog.error(s"Failure trying to send email, in $name", ex)
        }
      }

      future
    }
  }

  def getTransport(
    session: Session,
    retries: Int = 3
  )(implicit
    scheduler: Scheduler,
    ex: ExecutionContext
  ): Future[SMTPTransport] =
    futureWithRetries("getTransport") { session.getTransport("smtp").asInstanceOf[SMTPTransport] }

  def connect(
    transport: SMTPTransport
  )(implicit
    scheduler: Scheduler,
    ex: ExecutionContext
  ): Future[Unit] =
    futureWithRetries("connect") { transport.connect(username, password) }

  def sendMessage(
    msg: MimeMessage,
    transport: SMTPTransport
  )(implicit
    scheduler: Scheduler,
    ex: ExecutionContext
  ) =
    futureWithRetries("sendMessage") { transport.sendMessage(msg, msg.getAllRecipients) }

  /**
   * This returns the status code of the sent email, in case anybody cares.
   *
   * TBD: we might want to eventually do something smarter in case of failure.
   */
  def sendEmail(
    msgInfo: EmailMsg
  )(implicit
    scheduler: Scheduler,
    ex: ExecutionContext
  ): Future[Int] = {
    val SessionWrapper(session) = createSession()
    val EmailMsg(from, to, toName, senderName, subjectWiki, bodyWiki, footerWiki) = msgInfo
    val msg = new MimeMessage(session)
    msg.setFrom(new InternetAddress(from.addr, s"Querki for $senderName"))

    val replyAddrs = Array(new InternetAddress(from.addr, "Do not reply to this email").asInstanceOf[Address])
    msg.setReplyTo(replyAddrs)

    // TBD: there must be a better way to do this. It's a nasty workaround for the fact
    // that setRecipients() is invariant, I believe.
    val toAddrs = Array(new InternetAddress(to.addr, toName).asInstanceOf[Address])
    msg.setRecipients(Message.RecipientType.TO, toAddrs)

    msg.setSubject(subjectWiki.plaintext)

    val body =
      Wikitext("""{{maindiv:
                 |<div class="logocontainer"><img class="logo" alt="Querki Logo" src="http://querki.net/assets/images/Logo-green.png"></div>
                 |
                 |""".stripMargin) +
        bodyWiki +
        Wikitext("""
                   |}}
                   |
                   |{{footer:
                   |""".stripMargin) +
        footerWiki +
        Wikitext("""
                   |}}
                   |
                   |{{footerAddr:
                   |Querki Inc, 28 Murdock St. #B, Somerville, MA 02145
                   |}}""".stripMargin)

    // Attach the HTML...
    val bodyHtml = s"""<html>
                      |<head>
                      |$emailStylesheet
                      |</head>
                      |<body>
                      |${body.display}
                      |</body>
                      |</html>""".stripMargin
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
      val transportFut = getTransport(session)
      val returnCodeFut = for {
        transport <- transportFut
        _ <- connect(transport)
        _ <- sendMessage(msg, transport)
        _ = if (debug) QLog.spew(s"Sent email; transport returned ${transport.getLastServerResponse}")
      } yield transport.getLastReturnCode

      returnCodeFut.andThen {
        // When the whole thing finishes, regardless of what happens, make sure we release the Transport:
        case _ => transportFut.onSuccess { case transport => transport.close() }
      }
    } else {
      // Non-TLS -- running on a test server, so just do it the easy way:
      // TBD: we might eventually reallow this for test servers, but not for the time being:
//      Transport.send(msg)
      throw new Exception(s"Trying to send an email without TLS, which is no longer supported!")
    }
  }

  /**
   * DEPRECATED
   *
   * The guts of actually sending email. Note that this can be invoked from a number of different circumstances,
   * some user-initiated and some not.
   */
  def sendInternal(
    sessionWrapper: TSession,
    from: String,
    recipientEmail: EmailAddress,
    recipientName: String,
    requester: Identity,
    subject: Wikitext,
    bodyMain: Wikitext
  ): Try[Unit] =
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

      val body =
        Wikitext("""{{maindiv:
                   |<div class="logocontainer"><img class="logo" alt="Querki Logo" src="http://querki.net/assets/images/Logo-green.png"></div>
                   |
                   |""".stripMargin) +
          bodyMain +
          Wikitext("""
                     |}}
                     |
                     |{{footer:
                     |If you believe you have received this message in error, please drop a note to "betaemail@querki.net". (We're
                     |working on the one-click unsubscribe, but not quite there yet; sorry.)
                     |}}""".stripMargin)

      // Attach the HTML...
      val bodyHtml = s"""<html>
                        |<head>
                        |$emailStylesheet
                        |</head>
                        |<body>
                        |${body.display}
                        |</body>
                        |</html>""".stripMargin
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
