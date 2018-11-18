package querki.test.mid

import querki.globals._

/**
 * Functions for examining "email" that has been sent during mid-level tests.
 * 
 * This is, experimentally, being built in a less cake-like style. Note that all functions
 * require the Ecology be be implicitly available.
 */
object EmailTesting {
  final val validateLinkRegex = """\[([^\"]*)\]""".r.unanchored
  
  def emailInspector(implicit ecology: Ecology) = ecology.api[querki.email.TestEmailInspector] 

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
}
