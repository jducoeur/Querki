package querki.email

import scala.concurrent.Future

import models.Wikitext
import querki.data.TOID

trait EmailFunctions {
  import EmailFunctions._

  /**
   * Given the string from an Unsubscribe link, fetch the choices to present to the User.
   */
  def getUnsubOptionsFor(unsubStr: String): Future[UnsubPageInfo]

  /**
   * Takes the info from an UnsubOption, and actually does that Unsubscribe. Returns the message to
   * display to the user.
   */
  def unsubscribe(
    notifier: String,
    identityId: TOID,
    unsubId: TOID,
    context: Option[String]
  ): Future[Wikitext]
}

object EmailFunctions {

  /**
   * Describes one option the user might choose in the UnsubscribePage.
   */
  case class UnsubOption(
    notifier: String,
    unsubId: TOID,
    context: Option[String],
    label: String,
    desc: String
  )

  case class UnsubPageInfo(
    identityId: TOID,
    emailInfo: Wikitext,
    options: Seq[UnsubOption]
  )
}
