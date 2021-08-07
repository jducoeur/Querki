package querki.email

import models._
import querki.globals._
import querki.identity._
import querki.notifications.NotifierId
import querki.persistence._
import querki.util.{Hasher, SafeUrl, SignedHash}

case class UnsubscribeEvent(
  @KryoTag(1) notifierId: String,
  @KryoTag(2) details: UnsubEvent with UseKryo
) extends UseKryo

class UnsubscribeEcot(e: Ecology) extends QuerkiEcot(e) with Unsubscribe {

  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]

  override def persistentMessages = persist(67, (classOf[UnsubscribeEvent] -> 100))

  // TODO: this is duplicated in multiple Ecots. It really should be defined in one, and used in the rest:
  lazy val urlBase = Config.getString("querki.app.urlRoot")
  val unsubParam = "unsubInfo"

  def generateUnsubLink(
    notifier: EmailNotifier,
    identityId: IdentityId,
    email: EmailAddress,
    rest: String*
  ): String = {
    val idString = (List(
      notifier.id.toString,
      email.addr,
      identityId.toString
    )
      ++ rest).mkString(":")
    val signed = Hasher.sign(idString, emailSepChar)
    val encoded = SafeUrl(signed.toString)
    urlBase +
      "unsub" +
      "?" + unsubParam + "=" + encoded
  }

  def parseUnsub(encodedUnsub: String): Option[UnsubInfo] = {
    val hash = SignedHash(encodedUnsub, emailSepChar)
    if (!Hasher.checkSignature(hash))
      None
    else {
      val SignedHash(_, _, msg, _) = hash
      val msgComponents = msg.split(":").toList
      val notifierIdStr :: emailAddrStr :: identityIdStr :: rest = msgComponents
      val user =
        if (emailAddrStr.length == 0)
          IdentityAccess.makeTrivial(OID(identityIdStr))
        else
          IdentityAccess.makeGuest(identityIdStr, emailAddrStr)
      Some(
        UnsubInfo(
          NotifierId(notifierIdStr),
          user,
          EmailAddress(emailAddrStr),
          rest
        )
      )
    }
  }
}
