package querki.test.mid

import querki.email.TestEmailMessageDetails
import querki.globals._

/**
 * Functions for examining "email" that has been sent during mid-level tests.
 */
object EmailTesting {
  final val validateLinkRegex = """\[([^\"]*)\]""".r.unanchored
  final val inviteLinkRegex = 
    """<div class="bottomlinkdiv"><a href="([^\"]*)" class="btn btn-primary">Join Space""".r.unanchored

  def emailInspector(implicit ecology: Ecology) = ecology.api[querki.email.TestEmailInspector]
  
  /**
   * A notionally-opaque token that represents the next email to be sent.
   * 
   * This can then be used to handle the email once it goes out.
   */
  case class EmailListenerToken(private [EmailTesting] fut: Future[TestEmailMessageDetails])
  
  /**
   * Prep to handle the next email that gets sent. You must call this *before* calling the
   * function that will send the email!!!
   */
  def nextEmail: TestOp[EmailListenerToken] = TestOp.withState { state =>
    implicit val ecology = state.harness.ecology
    EmailListenerToken(emailInspector.handleNextEmail)
  }

  /**
   * This registers a function that will be run on email specified by the token. The email may
   * already be sent, or might be delayed; if delayed, this will block until it is sent.
   */
  def withNextEmail[R](token: EmailListenerToken)(f: TestEmailMessageDetails => R): TestOp[R] = TestOp.fut { state =>
    token.fut.map { msg =>
      (state, f(msg))
    }
  }

  /**
   * This returns the body of the most recently-sent email.
   */
  def fetchLatestEmailBody()(implicit ecology: Ecology): String = {
    val session = emailInspector.sessions.head
    val email = session.messages.head
    email.bodyMain.plaintext
  }

  /**
   * This fetches the validation link from the most recently-sent email.
   */
  def extractValidateLink()(implicit ecology: Ecology): String = {
    val body = fetchLatestEmailBody()
    body match {
      case validateLinkRegex(url) => url
      case _ => throw new Exception(s"Didn't find validation link in $body")
    }
  }
  
  /**
   * This fetches the validation hash out of the validation link in the most recently-sent email.
   */
  def extractValidateHash()(implicit ecology: Ecology): String = {
    val hashPre = "validate="
    val link = extractValidateLink()
    val hashPos = link.indexOf(hashPre) + hashPre.length
    link.substring(hashPos)
  }
  
  def extractInviteLink(token: EmailListenerToken): TestOp[String] = withNextEmail(token) { msg =>
    val body = msg.bodyMain.plaintext
    body match {
      case inviteLinkRegex(url) => url
      case _ => throw new Exception(s"Didn't find invitation link in $body")
    }
  }
  
  final val inviteHashPre = "invite="
  /**
   * This fetches the invitation hash out of the invite link in the email indicated by the token.
   */
  def extractInviteHash(token: EmailListenerToken): TestOp[String] = {
    for {
      link <- extractInviteLink(token)
      hashPos = link.indexOf(inviteHashPre) + inviteHashPre.length
    }
      yield link.substring(hashPos)
  }
}
