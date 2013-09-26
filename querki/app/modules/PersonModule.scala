package modules.person

// For talking to the SpaceManager:
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.duration._

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import querki.values._

import querki.identity._

import modules._
// TODO: Hmm. This is a Module-boundaries break. I think we need an interface layer:
import modules.email.EmailAddress

import querki.util._

import controllers.{Contributor, PageEventManager, Publisher, RequestContext}

import play.api.Logger

  
// TODO: this Module should formally depend on the Email Module. Probably --
// it isn't entirely clear whether statically described Properties really
// require initialization-order dependencies. But I believe that the Person
// object shouldn't be constructed until after the Email Module has been.
class PersonModule(val moduleId:Short) extends modules.Module {

  object MOIDs {
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
  import MOIDs._
  
  override def init = {
    PageEventManager.requestReceived += IdentityLoginChecker
    PageEventManager.requestReceived += InviteLoginChecker
  }
  
  override def term = {
    PageEventManager.requestReceived -= InviteLoginChecker
    PageEventManager.requestReceived -= IdentityLoginChecker
  }

  /***********************************************
   * EXTERNAL REFS
   ***********************************************/

  lazy val Email = modules.Modules.Email
  lazy val emailAddressProp = Email.emailAddress
  lazy val emailAddressOID = Email.MOIDs.EmailPropOID
  
  lazy val AccessControl = modules.Modules.AccessControl
  
  lazy val urlBase = Config.getString("querki.app.urlRoot")
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  // The actual definition of this method is down below
  lazy val inviteLink = new SingleThingMethod(InviteLinkCmdOID, "Invite Link", "Generate a Link to invite someone to join this Space.", 
      """Place this command inside of an Email Message.
When the email is sent, it will be replaced by a link that the recipient of the email can use to log into this Space as a Person.""", doInviteLink(false))

  lazy val chromelessInviteLink = new SingleThingMethod(ChromelessInviteLinkOID, "Plain Invite Link", "Generate a Link to join this Space, with no Querki menus and such.", 
      """Place this command inside of an Email Message.
When the email is sent, it will be replaced by a link that the recipient of the email can use to log into this Space as a Person.
Unlike the ordinary Invite Link command, this one results in a page with no Querki menu bar, just your pages.
(NOTE: this will probably become a paid-users-only feature in the future. Also, this method isn't usually what you want any more;
instead, you usually want to set the Chromeless Invites property on your Space.)""", doInviteLink(true))

  lazy val identityLink = new SystemProperty(IdentityLinkOID, LinkType, Optional,
      toProps(
        setName("Person to Identity Link"),
        InternalProp(true),
        PropSummary("INTERNAL: points from a Space-scoped Person to a System-scoped Identity")))

  lazy val meMethod = new InternalMethod(MeMethodOID,
      toProps(
        setName("_me"),
        PropSummary("If the current user is a Person in the current Space, return that Person"),
        PropDetails("""_me is the usual way to customize a Space based on who is looking at it. If the page is being viewed by
            |a logged-in User, *and* they are a Member of this Space, it produces their Person record. If the viewer isn't
            |logged in, or isn't a Member, this will produce a Warning.
            |
            |NOTE: the high concept of _me is important, and will be continuing, but the details are likely to evolve a great
            |deal, to make it more usable. So don't get too invested in the current behaviour.""".stripMargin)))
  {
    override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
      val userOpt = context.request.requester
      val personOpt = userOpt.flatMap { user =>
        user match {
          case SpaceSpecificUser(identityId, name, email, spaceId, personId) => Some(LinkValue(personId))
          case _ => None
        }
      }
      personOpt.getOrElse(WarningValue("Not logged into this Space"))
    }
  }
  
  lazy val chromelessInvites = new SystemProperty(ChromelessInvitesOID, YesNoType, ExactlyOne,
      toProps(
        setName("Chromeless Invites"),
        PropSummary("Should invitees to this Space see it unadorned with Querki chrome?"),
        PropDetails("""If you set this to Yes on a Space or Thing, then Invite Links pointing
            |to that will show up without Querki chrome. That is, when they join, they'll just see your
            |pages, with no top menu or Querki footer.
            |
            |This feature is mainly intended for "white-labeling" Spaces, so that they don't look
            |as Querki-ish. We make it available for Spaces that care a great deal about how they look.
            |(It was originally designed for wedding invitations.)
            |
            |NOTE: this will probably become a paid-users-only feature in the future.""".stripMargin)))
  
  lazy val inviteText = new SystemProperty(InviteTextOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Space Invitation Text"),
        AppliesToKindProp(Kind.Space),
        PropSummary("The text to use when inviting people to join your Space"),
        PropDetails("""This is the content of the invitation email, to go along with the standard Querki
            |invitation text.
            |
            |This is included in the Sharing and Security page, so you don't usually need to do anything
            |directly with it.""".stripMargin)))

  override lazy val props = Seq(
    inviteLink,
    
    chromelessInviteLink,
    
    identityLink,
    
    meMethod,
    
    chromelessInvites,
    
    inviteText,
    
    spaceInvite
  )
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val securityPrincipal = ThingState(SecurityPrincipalOID, systemOID, RootOID,
      toProps(
        setName("Security Principal"),
        DisplayTextProp("""For internal use -- this the concept of a Thing that can be given permissions.""")))
  
  lazy val person = ThingState(PersonOID, systemOID, SecurityPrincipalOID,
      toProps(
        setName("Person"),
        IsModelProp(true),
        // TODO: this is a fugly declaration, and possibly unsafe -- do we have any
        // assurance that modules.Modules.Email has been constructed before this?
        (modules.Modules.Email.MOIDs.EmailPropOID -> Optional.QNone),
        DisplayTextProp("""
This represents a Person who is using Querki or can be invited to it. You can create a Person in
your Space, and compose an email to invite them to use the Space; you can also create a new Model
to add new Properties for any Person in your Space.
""")))
    
  override lazy val things = Seq(
    securityPrincipal,
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
  private def setIdentityId(t:Thing, context:QLContext):OID = {
    implicit val s = context.state
    val emailAddr = t.first(emailAddressProp)
    val name = t.first(NameProp)
    // Get the Identity in the database...
    val identity = Identity.getOrCreateByEmail(emailAddr, name)
    // ... then point the Person to it, so we can use it later...
    val req = context.request
    // TODO: we shouldn't do this again if the Person is already pointing to the Identity:
    val changeRequest = ChangeProps(req.requester.get, s.owner, s.id, t.toThingId, toProps(identityLink(identity.id))())
    // TODO: eventually, all of this should get beefed up in various ways:
    // -- ChangeProps should carry the version stamp of t, so that race conditions can be rejected.
    // -- Ideally, we shouldn't send the email until the Identity has been fully established. Indeed, this whole
    //    business really *ought* to be transactional.
    SpaceManager.ask(changeRequest) { resp:ThingResponse =>
      resp match {
        case ThingFound(id, state) => Logger.info("Added identity " + identity.id + " to Person " + t.toThingId)
        case ThingFailed(error, msg, stateOpt) => Logger.error("Unable to add identity " + identity.id + " to Person " + t.toThingId + ": " + msg)
      }
    }
    // ... and finally, return it so we can use it now:
    identity.id
  }
  
  def idHash(personId:OID, identityId:OID):EncryptedHash = {
    Hasher.calcHash(personId.toString + identityId.toString)
  }
  
  val identityParam = "identity"
  val identityName = "identityName"
  val identityEmail = "identityEmail"
  val personParam = "person"
  val inviteParam = "invite"
    
  def doInviteLink(chromelessIn:Boolean)(t:Thing, context:QLContext):QValue = {
    implicit val state = context.state
    val personOpt =
      for {
        rootId <- context.root.value.firstAs(LinkType);
        rootThing <- state.anything(rootId);
        if (rootThing.isAncestor(PersonOID))
      }
        yield rootThing
        
    personOpt match {
      case Some(person) => {
        val chromeless = chromelessIn || t.ifSet(chromelessInvites) || state.ifSet(chromelessInvites)
        // Get the Identity linked from this Person. If there isn't already one, make one.
	    val identityProp = person.localProp(identityLink)
	    val identityId = identityProp match {
	      case Some(propAndVal) if (!propAndVal.isEmpty) => propAndVal.first
  	      // This will set identityProp, as well as getting the Identity's OID:
	      case _ => setIdentityId(person, context)
	    }
	    val hash = idHash(person.id, identityId)
	    
	    // TODO: this surely belongs in a utility somewhere -- it constructs the full path to a Thing, plus some paths.
	    // Technically speaking, we are converting a Link to an ExternalLink, then adding params.
	    val url = urlBase + "u/" + state.owner.toThingId + "/" + state.name + "/" + t.toThingId + 
	      "?" + personParam + "=" + person.id.toThingId + "&" +
	      identityParam + "=" + hash +
	      (if (chromeless) "&cl=on" else "")
	    ExactlyOne(ExternalLinkType(url))
      }
      case _ => WarningValue("Invite Link is only defined when sending email")
    }
  }
  
  /***********************************************
   * LOGIN HANDLER
   ***********************************************/
  
  case class SpaceSpecificUser(identityId:OID, name:String, email:EmailAddress, spaceId:OID, personId:OID) extends User {
    val id = UnknownOID
    val identity = Identity(identityId, email, "", "", name, IdentityKind.SimpleEmail)
    val identities = Seq(identity)
    val level = UserLevel.SpaceSpecific
  }
  
  /**
   * This is called via callbacks when we are beginning to render a page. It looks to see whether the
   * URL or the Session contains a space-specific Identity that we should be using as the "user".
   * 
   * TODO: make this Identity Space-specific! It should be possible to have different Persons in the
   * Session for different Spaces.
   */
  object IdentityLoginChecker extends Contributor[RequestContext,RequestContext] {
    def notify(rc:RequestContext, sender:Publisher[RequestContext, RequestContext]):RequestContext = {
      val rcOpt =
        for (
          idParam <- rc.firstQueryParam(identityParam);
          state <- rc.state;
          // NOTE: this is messy for backward compatibility. The first clause is the current way things work: the candidate is
          // the value of the "person" param. The orElse is the way it originally worked: the candidate is the Thing being pointed to.
          // TODO: this should be deprecated and removed when the Wedding Site is done with, if not sooner.
          candidate <- rc.firstQueryParam(personParam).flatMap(personThingId => state.anything(ThingId(personThingId))) orElse rc.thing;
          idProp <- candidate.localProp(identityLink);
          emailPropVal <- candidate.getPropOpt(emailAddressProp)(state);
          email = emailPropVal.first;
          name = candidate.displayName;
          identityId = idProp.first;
          if Hasher.authenticate(candidate.id.toString + identityId.toString, EncryptedHash(idParam));
          updates = Seq((identityParam -> identityId.toString), (identityName -> name), (identityEmail -> email.addr), (personParam -> candidate.id.toString));
          // TODO: if there is already a User in the RC, we should *add* to that User rather than
          // replacing it:
          newRc = rc.copy(
              sessionUpdates = rc.sessionUpdates ++ updates,
              requester = Some(SpaceSpecificUser(identityId, name, email, state.id, candidate.id)))
        ) 
          yield newRc
          
      rcOpt.getOrElse {
        // Okay, the URL doesn't have an Identity login. Does the session already have one?
        val session = rc.request.session
        val withIdentityOpt = for (
          existingIdentity <- session.get(identityParam);
          idName <- session.get(identityName);
          idEmail <- session.get(identityEmail);
          personId <- session.get(personParam)
          )
          yield rc.copy(requester = Some(SpaceSpecificUser(OID(existingIdentity), idName, EmailAddress(idEmail), rc.state.get.id, OID(personId))))
        withIdentityOpt.getOrElse(rc)
      }
    }
  }
  
  /*************************************************************
   * INVITATION MANAGEMENT
   *************************************************************/

  // TODO: this belongs in a utility library somewhere:
  def encodeURL(url:String):String = java.net.URLEncoder.encode(url, "UTF-8")
  def decodeURL(url:String):String = java.net.URLDecoder.decode(url, "UTF-8")
  
  lazy val spaceInvite = new SingleContextMethod(SpaceInviteOID,
      toProps(
        setName("_spaceInvitation"), 
        InternalProp(true),
        PropSummary("Generate a Link to invite someone to join this Space."), 
        PropDetails("""This is intended for internal use only. It is used to generate
            |the link that is sent to invitees to your Space""".stripMargin)))
  {
    def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
      implicit val state = mainContext.state
      
      // TODO: we're using EmailModule to inject the Person as the root context. That's ghastly, and
      // no longer necessary: add a backdoor to add the Person as an annotation instead.
      val inviteOpt =
        for {
          rootId <- mainContext.root.value.firstAs(LinkType);
          person <- state.anything(rootId);
          if (person.isAncestor(PersonOID));
          emailPropOpt <- person.getPropOptTyped(emailAddressProp);
          email <- emailPropOpt.firstOpt;
          rc <- mainContext.requestOpt;
          // This is the value that we're going to use to identify this person when they
          // click on the link. We need to include the email address, to guard against it
          // being changed in the Person record after the email is sent. (Which could be
          // used as a spam vector, I suspect.)
          idString = person.id.toString + ":" + email.addr;
          signed = Hasher.sign(idString, Email.emailSepChar);
          encoded = encodeURL(signed.toString);
          // TODO: this surely belongs in a utility somewhere -- it constructs the full path to a Thing, plus some paths.
	      // Technically speaking, we are converting a Link to an ExternalLink, then adding params.
	      url = urlBase + "u/" + rc.ownerHandle + "/" + state.toThingId +
	        "/?" + inviteParam + "=" + encoded
        }
        yield HtmlValue(s"""<b><a href="$url">Click here</a></b> to accept the invitation.""")
        
      inviteOpt.getOrElse(WarningValue("This appears to be an incorrect use of _spaceInvitation."))
    }
  }
  
  case class InvitationResult(invited:Seq[EmailAddress], alreadyInvited:Seq[EmailAddress])
  
  /**
   * Invite some people to join this Space. rc.state must be established (and authentication dealt with) before
   * we get here.
   * 
   * TODO: restructure this to be Async!
   */
  def inviteMembers(rc:RequestContext, invitees:Seq[EmailAddress]):InvitationResult = {
    val originalState = rc.state.get
    
    // Filter out any of these email addresses that have already been invited:
    val currentMembers = originalState.descendants(PersonOID, false, true)
    // TODO: this is doing an n^2 check of whether the email addresses already exist. Rewrite to be less
    // inefficient using a Set:
    val currentEmails = currentMembers.flatMap(_.getPropOptTyped(emailAddressProp)(originalState)).map(_.first).toSeq
    val (existingEmails, newEmails) = invitees.partition(newEmail => currentEmails.exists(oldEmail => modules.Modules.Email.EmailAddressType.doMatches(newEmail, oldEmail)))
    
    // Create Person records for all the new ones:
    // TODO: add a CreateThings message that allows me to do multi-create:
    // TODO: this is an icky side-effectful way of getting the current state:
    var updatedState = originalState
    val people = newEmails.map { address =>
      // Create a Display Name. No, we're not going to worry about uniqueness -- Display Names are allowed
      // to be duplicated:
      val prefix = address.addr.takeWhile(_ != '@')
      val displayName = "Invitee " + prefix
      
      val propMap = 
        Thing.toProps(
          emailAddressProp(address.addr),
          DisplayNameProp(displayName),
          AccessControl.canReadProp(AccessControl.ownerTag))()
      val msg = CreateThing(rc.requester.get, rc.ownerId, updatedState.toThingId, Kind.Thing, PersonOID, propMap)
      implicit val timeout = Timeout(5 seconds)
      val future = SpaceManager.ref ? msg
      // TODO: EEEEVIL! This code is quick and dirty but wrong. This should all be Async, using SpaceManager.ask:
      Await.result(future, 5 seconds) match {
        case ThingFound(id, newState) => { updatedState = newState; newState.anything(id).get }
        case err => throw new Exception("Error trying to create an invitee: " + err) // TODO: improve this error path!
      }
    }
    
    implicit val finalState = updatedState
    val newRc = rc.copy(state = Some(updatedState))
    val context = updatedState.thisAsContext(newRc)
    val subjectQL = QLText(rc.ownerName + " has invited you to join the Space " + updatedState.displayName)
    val inviteLink = QLText("""
      |
      |------
      |
      |[[_spaceInvitation]]""".stripMargin)
    val bodyQL = updatedState.getPropOpt(inviteText).flatMap(_.firstOpt).getOrElse(QLText("")) + inviteLink
    // TODO: we probably should test that sentTo includes everyone we expected:
    val sentTo = Email.sendToPeople(context, people, subjectQL, bodyQL)
    
    InvitationResult(newEmails, existingEmails)
  }
  
  /**
   * This is called via callbacks when we are beginning to render a page. It looks to see whether the
   * URL is an invitation to join this Space, and goes to the Invitation workflow if so.
   */
  object InviteLoginChecker extends Contributor[RequestContext,RequestContext] {
    def notify(rc:RequestContext, sender:Publisher[RequestContext, RequestContext]):RequestContext = {
      val rcOpt =
        for (
          encodedInvite <- rc.firstQueryParam(inviteParam);
          state <- rc.state;
          hash = SignedHash(encodedInvite, Email.emailSepChar);
          // TODO: we should do something smarter if this fails:
          if (Hasher.checkSignature(hash));
          SignedHash(_, _, msg, _) = hash;
          Array(personIdStr, emailAddrStr, _*) = msg.split(":");
          personId = OID(personIdStr);
          emailAddr = EmailAddress(emailAddrStr);
          updates = Map((personParam -> personIdStr), (identityEmail -> emailAddrStr))
        )
          yield rc.copy(sessionUpdates = rc.sessionUpdates ++ updates,
              redirectTo = Some(controllers.routes.Application.handleInvite(rc.ownerHandle, rc.state.get.toThingId)))
              
      // This gets picked up in Application.withSpace(), and redirected as necessary.
      rcOpt.getOrElse(rc)
    }
  }
}