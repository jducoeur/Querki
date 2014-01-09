package querki

import models.{OID, Property, Thing}
import models.system.QLText

import querki.ecology._

import querki.email.EmailAddress

import querki.values.{RequestContext, SpaceState}

package object identity {

  object MOIDs extends EcotIds(3)  {
    val PersonOID = oldMoid(1)
    val InviteLinkCmdOID = oldMoid(2)
    val IdentityLinkOID = oldMoid(3)
    val ChromelessInviteLinkOID = oldMoid(4)
    val MeMethodOID = oldMoid(5)
    val SecurityPrincipalOID = oldMoid(6)
    val ChromelessInvitesOID = moid(7)
    val InviteTextOID = moid(8)
    val SpaceInviteOID = moid(9)
  }

  case class InvitationResult(invited:Seq[EmailAddress], alreadyInvited:Seq[EmailAddress])
  
  // The cookie parameter that indicates the email address of the target identity. The
  // fact that we have to expose this here suggests we have an abstraction break to fix...
  val identityEmail = "identityEmail"
  val personParam = "person"
  
  trait Person extends EcologyInterface {
    def SecurityPrincipal:Thing
    def PersonModel:Thing
    
    def IdentityLink:Property[OID,OID]
    def InviteText:Property[QLText, String]    
   
    def inviteMembers(rc:RequestContext, invitees:Seq[EmailAddress]):InvitationResult
    
    // TODO: this is a horrible abstraction break. Do we really need PlayRequestContext here? Odds are
    // that this method doesn't belong in Person at all, given that *all* of its parameters involve weird
    // imports:
    def acceptInvitation[B](rc:controllers.PlayRequestContext)(cb:querki.spaces.messages.ThingResponse => B):Option[scala.concurrent.Future[B]]
    
    def getPersonIdentity(person:Thing)(implicit state:SpaceState):Option[OID]
    def hasPerson(user:User, personId:OID)(implicit state:SpaceState):Boolean
    def hasPerson(user:User, person:Thing)(implicit state:SpaceState):Boolean
    def isPerson(identity:Identity, person:Thing)(implicit state:SpaceState):Boolean
    def localPerson(identity:Identity)(implicit state:SpaceState):Option[Thing]
  }
}