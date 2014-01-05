package querki

import models.Property

import modules.ModuleIds

// For the HACKs below:
import scala.util.Try
import models.{OID, Thing, Wikitext}
import models.system.QLText
import querki.identity.Identity
import querki.values.{QLContext, SpaceState}

import querki.ecology._

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
  
  /**
   * The character that we use to separate strings involving email addresses. Chosen mostly
   * because it is not a legal character in email addresses.
   */
  val emailSepChar = ';'
    
  /**
   * Represents an email address. For the moment this is basically just a String, but we'll gradually
   * add things like validation, so it's useful to get the abstraction clean now.
   */
  case class EmailAddress(addr:String)
    
  trait Email extends EcologyInterface {  
    /**
     * The email address that ordinary user emails will come from.
     */
    def from:String
    
    /**
     * Sends an "official" email in the name of Querki itself.
     * 
     * USE WITH CAUTION! This should only be used for significant emails that do not come from a
     * specific person. Excessive use of these could get us labeled as spammers, which we don't want.
     * 
     * TODO: in the long run, this should wind up mostly subsumed under a more-general Notifications
     * system, which will include, eg, system-displayed notifications and social network messages,
     * with the user controlling where his notifications go.  
     */
    def sendSystemEmail(recipient:Identity, subject:Wikitext, body:Wikitext):Try[Unit]
    
    def sendToPeople(context:QLContext, people:Seq[Thing], subjectQL:QLText, bodyQL:QLText)(implicit state:SpaceState):Seq[OID]
    
    def EmailAddressProp:Property[EmailAddress,String]
  }
}