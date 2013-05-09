package modules.person

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import identity._

import modules._

import querki.util._

import controllers.{Contributor, PageEventManager, Publisher, RequestContext}

// TODO: this Module should formally depend on the Email Module. Probably --
// it isn't entirely clear whether statically described Properties really
// require initialization-order dependencies. But I believe that the Person
// object shouldn't be constructed until after the Email Module has been.
class PersonModule(val moduleId:Short) extends modules.Module {

  object MOIDs {
    val PersonOID = moid(1)
    val InviteLinkCmdOID = moid(2)
    val IdentityLinkOID = moid(3)
  }
  import MOIDs._
  
  override def init = {
    PageEventManager.requestReceived += IdentityLoginChecker
  }
  
  override def term = {
    PageEventManager.requestReceived -= IdentityLoginChecker
  }

  /***********************************************
   * EXTERNAL REFS
   ***********************************************/

  lazy val emailAddressProp = Modules.Email.emailAddress
  
  lazy val urlBase = Config.getString("querki.app.urlRoot")
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  // The actual definition of this method is down below
  lazy val inviteLink = new SingleThingMethod(InviteLinkCmdOID, "Invite Link", """Place this command inside of an Email Message.
When the email is sent, it will be replaced by a link that the recipient of the email can use to log into this Space as a Person.""", doInviteLink)
  
  lazy val identityLink = new SystemProperty(IdentityLinkOID, LinkType, Optional,
      toProps(
        setName("Person to Identity Link"),
        InternalProp(true),
        DisplayTextProp("INTERNAL: points from a Space-scoped Person to a System-scoped Identity")))

  override lazy val props = Seq(
    inviteLink,
    
    identityLink
  )
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val person = ThingState(PersonOID, systemOID, RootOID,
      toProps(
        setName("Person"),
        IsModelProp(true),
        // TODO: this is a fugly declaration, and possibly unsafe -- do we have any
        // assurance that modules.Modules.Email has been constructed before this?
        (modules.Modules.Email.MOIDs.EmailPropOID -> Optional.None),
        DisplayTextProp("""
This represents a Person who is using Querki or can be invited to it. You can create a Person in
your Space, and compose an email to invite them to use the Space; you can also create a new Model
to add new Properties for any Person in your Space.
""")))
    
  override lazy val things = Seq(
    // The Person Model
    person
  )
   
  /***********************************************
   * METHOD CONTENTS
   ***********************************************/
  
  /**
   * Find the Identity that goes with this Person's email address, or create one if there
   * isn't already one.
   */
  private def setIdentityId(t:Thing, context:ContextBase):OID = {
    implicit val s = context.state
    val emailAddr = t.first(emailAddressProp)
    val name = t.first(NameProp)
    val identity = Identity.getOrCreateByEmail(emailAddr, name)
    identity.id
  }
  
  def idHash(personId:OID, identityId:OID):EncryptedHash = {
    Hasher.calcHash(personId.toString + identityId.toString)
  }
  
  val identityParam = "identity"
    
  def doInviteLink(t:Thing, context:ContextBase):TypedValue = {
    if (t.isAncestor(PersonOID)(context.state)) {
      // Get the Identity linked from this Person. If there isn't already one, make one.
	  val identityProp = t.localProp(identityLink)
	  val identityId = identityProp match {
	    case Some(propAndVal) => propAndVal.first
	    case None => setIdentityId(t, context)
	  }
	  val hash = idHash(t.id, identityId)
	    
	  val state = context.state
	  // TODO: this surely belongs in a utility somewhere -- it constructs the full path to a Thing, plus some paths.
	  val url = urlBase + "u/" + state.owner.toThingId + "/" + state.name + "/" + t.toThingId + "?" + identityParam + "=" + hash
	  val link = ExactlyOne(ExternalLinkType(url))
	    
	  TypedValue(link, ExternalLinkType)
	} else {
	  TextValue("Invite Link is only defined when sending email")
	}
  }
  
  /***********************************************
   * LOGIN HANDLER
   ***********************************************/
  
  object IdentityLoginChecker extends Contributor[RequestContext,RequestContext] {
    def notify(rc:RequestContext, sender:Publisher[RequestContext, RequestContext]):RequestContext = {
      // TODO: update the session if there's an Identity flag
      rc
    }
  }
}