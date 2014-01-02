package querki

import models.PropertyInterface

import modules.ModuleIds

// For the HACKs below:
import scala.util.Try
import models.{OID, Thing, Wikitext}
import models.system.QLText
import querki.identity.Identity
import querki.values.{QLContext, SpaceState}

package object email {
  object MOIDs extends ModuleIds(2) {
    val EmailTypeOID = oldMoid(1)
    val EmailPropOID = oldMoid(2)
    val EmailTemplateOID = oldMoid(3)
    val EmailToOID = oldMoid(4)
    val EmailSendOID = oldMoid(5)
    val EmailSubjectOID = oldMoid(6)
//    val EmailCcOID = moid(7)
    val EmailBodyOID = oldMoid(8)
    val EmailShowSendOID = oldMoid(9)
    val SentToOID = oldMoid(10)
    val RecipientsOID = oldMoid(11)
  }
  
  val EmailAddressProp = new PropertyInterface[EmailAddress,String]
  
  /**
   * The character that we use to separate strings involving email addresses. Chosen mostly
   * because it is not a legal character in email addresses.
   */
  val emailSepChar = ';'
    
  def from:String = {
    _from match {
      case Some(f) => f
      case _ => throw new Exception("Email.from hasn't been initialized!")
    }
  }
  private var _from:Option[String] = None
  def setFrom(f:String) = { _from = Some(f) }
    
  /**
   * Represents an email address. For the moment this is basically just a String, but we'll gradually
   * add things like validation, so it's useful to get the abstraction clean now.
   */
  case class EmailAddress(addr:String)
  
  /**
   * HACK: this is a temporary workaround to decouple this call. It should be replaced by a more
   * general Notifications system, which the Email system will hook itself into, but for now we
   * do that registration as a hack.
   */
  def sendSystemEmail(recipient:Identity, subject:Wikitext, body:Wikitext):Try[Unit] = {
    emailCB match {
      case Some(cb) => cb(recipient, subject, body)
      case _ => throw new Exception("sendSystemEmail not registered yet!")
    }
  }
  private var emailCB:Option[(Identity, Wikitext, Wikitext) => Try[Unit]] = None
  def setEmailCallback(cb:(Identity, Wikitext, Wikitext) => Try[Unit]) = {
    emailCB = Some(cb)
  }
  
  /**
   * HACK: same as above.
   */
  def sendToPeople(context:QLContext, people:Seq[Thing], subjectQL:QLText, bodyQL:QLText)(implicit state:SpaceState):Seq[OID] = {
    emailPeopleCB match {
      case Some(cb) => cb(context, people, subjectQL, bodyQL, state)
      case _ => throw new Exception("sendToPeople not registered yet!")
    }    
  }
  private var emailPeopleCB:Option[(QLContext, Seq[Thing], QLText, QLText, SpaceState) => Seq[OID]] = None
  def setEmailPeopleCallback(cb:(QLContext, Seq[Thing], QLText, QLText, SpaceState) => Seq[OID]) = {
    emailPeopleCB = Some(cb)
  }
}