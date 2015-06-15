package querki.identity

// For talking to the SpaceManager:
import akka.pattern._
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global 
import scala.concurrent.Future

import models._

import querki.core.QLText
import querki.ecology._
import querki.email.emailSepChar
import querki.globals._
import querki.ql.QLPhrase
import querki.spaces.{CacheUpdate, SpaceManager}
import querki.spaces.messages.{ChangeProps, CreateThing, ThingError, ThingFound, ThingResponse}
import querki.util.{Contributor, Publisher, Hasher, SignedHash}
import querki.values._

import querki.identity._
import querki.email.EmailAddress

import controllers.{PageEventManager, PlayRequestContext}

import play.api.Logger
  
import MOIDs._
  
/**
 * This is an internal cache of the People in a Space (that is to say, all Person records), indexed
 * by PersonId and IdentityId. It is stored in the Space's cache, and is private to the Identity system.
 */
private [identity] case class CachedPeople(val ecology:Ecology, state:SpaceState) extends EcologyMember {
  
  implicit val s = state
  implicit val e = ecology
  
  lazy val Person = interface[Person]
  
  private def getPersonIdentityRaw(person:Thing):Option[OID] = {
    for (
      identityVal <- person.getPropOpt(Person.IdentityLink);
      identityId <- identityVal.firstOpt
        )
      yield identityId  
  }     
  
  val (peopleById, peopleByIdentityId) = 
    ((Map.empty[OID, Thing], Map.empty[IdentityId, Thing]) /: state.descendants(PersonOID, false, true)) { (maps, person) =>
      val (personIdMap, identityIdMap) = maps
      val newPersonIdMap = personIdMap + (person.id -> person)
      val newIdentityIdMap = getPersonIdentityRaw(person).map(identityId => identityIdMap + (identityId -> person)).getOrElse(identityIdMap)
      (newPersonIdMap, newIdentityIdMap)
    }
    
  def allPeople = peopleById.values
  def hasPerson(id:IdentityId) = peopleByIdentityId.contains(id)
  def localPerson(id:IdentityId) = peopleByIdentityId.get(id)
}

/**
 * TODO: this should probably be split into two modules, with all of the HTTP-specific stuff
 * surrounding Cookies brought into the controllers instead. But it'll do for now.
 */
class PersonModule(e:Ecology) extends QuerkiEcot(e) with Person with querki.core.MethodDefs with Contributor[CacheUpdate, CacheUpdate] {
  
  val Email = initRequires[querki.email.Email]
  val Basic = initRequires[querki.basic.Basic]
  val PageEventManager = initRequires[controllers.PageEventManager]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  lazy val Links = interface[querki.links.Links]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Profiler = interface[querki.tools.Profiler]
  lazy val Roles = interface[querki.security.Roles]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  lazy val EmailAddressProp = Email.EmailAddressProp
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  lazy val ExternalLinkType = Links.URLType
  
  lazy val prof = Profiler.createHandle("Person")
  
  override def init = {
    PageEventManager.requestReceived += InviteLoginChecker
    SpaceChangeManager.updateStateCache += this
  }
  
  override def term = {
    PageEventManager.requestReceived -= InviteLoginChecker
    SpaceChangeManager.updateStateCache -= this
  }
  
  object StateCacheKeys {
    val people = "People"
  }
  val peopleKey = StateCacheKey(MOIDs.ecotId, StateCacheKeys.people)
  
  /**
   * This gets called whenever a SpaceState is updated. We take that opportunity to build up a cached
   * mapping of Properties by SkillLevel.
   */
  def notify(evt:CacheUpdate, sender:Publisher[CacheUpdate, CacheUpdate]):CacheUpdate = {
    implicit val state = evt.current
    val cache:CachedPeople = CachedPeople(ecology, state)
        
    evt.updateCacheWith(MOIDs.ecotId, StateCacheKeys.people, cache)
  }
  
  def withCache[T](f:CachedPeople => T)(implicit state:SpaceState):T = {
    state.cache.get(peopleKey) match {
      case Some(cache @ CachedPeople(_, _)) => { 
        f(cache)
      }
      case other => throw new Exception(s"Didn't find CachedPeople for space ${state.id} -- got $other instead!")
    }
  }
  
  /**
   * All the people who have been invited into this Space.
   */
  def people(implicit state:SpaceState):Iterable[Thing] = {
    withCache(_.allPeople)
  }
  /**
   * All the people who have been invited into this Space who have not yet accepted.
   */
  def invitees(implicit state:SpaceState):Iterable[Thing] = people.filterNot(_.hasProp(IdentityLink))
  /**
   * All the people who have joined this Space.
   */
  def members(implicit state:SpaceState):Iterable[Thing] = people.filter(_.hasProp(IdentityLink))

  /***********************************************
   * EXTERNAL REFS
   ***********************************************/

  lazy val AccessControl = interface[querki.security.AccessControl]
  
  lazy val urlBase = Config.getString("querki.app.urlRoot")
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  lazy val IdentityLink = new SystemProperty(IdentityLinkOID, LinkType, Optional,
      toProps(
        setName("Person to Identity Link"),
        Core.InternalProp(true),
        Summary("INTERNAL: points from a Space-scoped Person to a System-scoped Identity")))
  
  lazy val InviteText = new SystemProperty(InviteTextOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Space Invitation Text"),
        AppliesToKindProp(Kind.Space),
        Summary("The text to use when inviting people to join your Space"),
        Details("""This is the content of the invitation email, to go along with the standard Querki
            |invitation text.
            |
            |This is included in the Sharing and Security page, so you don't usually need to do anything
            |directly with it.""".stripMargin)))
      
  /***********************************************
   * FUNCTIONS
   ***********************************************/

  lazy val meMethod = new InternalMethod(MeMethodOID,
      toProps(
        setName("_me"),
        Summary("If the current user is a Person in the current Space, return that Person"),
        Details("""_me is the usual way to customize a Space based on who is looking at it. If the page is being viewed by
            |a logged-in User, *and* they are a Member of this Space, it produces their Person record. If the viewer isn't
            |logged in, or isn't a Member, this will produce a Warning.
            |
            |NOTE: the high concept of _me is important, and will be continuing, but the details are likely to evolve a great
            |deal, to make it more usable. So don't get too invested in the current behaviour.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      
      val userOpt = context.request.requester
      implicit val state = context.state
      val personOpt = userOpt.flatMap(localPerson(_))
      personOpt.map(person => Links.LinkValue(person)).getOrElse(QL.WarningValue("You are not a member of this Space"))
    }
  }
  
  /**
   * TODO: in order to run efficiently, this function cheats -- it builds a PublicIdentity from the information in
   * the Person record, rather than actually going to the IdentityCache. In theory this should work; in practice, it's
   * a bit suspicious. Once QL is able to cope better with asynchronous functions, rewrite this to be more honest.
   */
  lazy val PersonIdentityFunction = new InternalMethod(PersonIdentityFunctionOID,
    toProps(
      setName("_personIdentity"),
      Summary("Given a Person (such as a Member), this produces the Identity corresponding to that Person")))
  {
    override def qlApply(inv:Invocation):QValue = {
      implicit val s = inv.state
      for {
        person <- inv.contextAllThings
        identityPV <- inv.opt(person.getPropOpt(IdentityLink))
        identityId <- inv.opt(identityPV.firstOpt)
        name = person.displayName
      }
        yield ExactlyOne(IdentityAccess.IdentityType(SimpleIdentity(identityId, name, name)))
    }
  }

  override lazy val props = Seq(
    IdentityLink,
    InviteText,
    spaceInvite,
    
    meMethod,
    PersonIdentityFunction
  )

  /*************************************************************
   * INVITATION MANAGEMENT
   *************************************************************/

  val inviteParam = "invite"
    
  // TODO: this belongs in a utility library somewhere:
  def encodeURL(url:String):String = java.net.URLEncoder.encode(url, "UTF-8")
  def decodeURL(url:String):String = java.net.URLDecoder.decode(url, "UTF-8")
  
  lazy val spaceInvite = new InternalMethod(SpaceInviteOID,
      toProps(
        setName("_spaceInvitation"), 
        Core.InternalProp(true),
        Summary("Generate a Link to invite someone to join this Space."), 
        Details("""This is intended for internal use only. It is used to generate
            |the link that is sent to invitees to your Space""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      val mainContext = inv.context
      implicit val state = inv.state
      
      // TODO: we're using EmailModule to inject the Person as the root context. That's ghastly, and
      // no longer necessary: add a backdoor to add the Person as an annotation instead.
      val inviteOpt =
        for {
          rootId <- mainContext.root.value.firstAs(LinkType);
          person <- state.anything(rootId);
          if (person.isAncestor(PersonOID));
          emailPropOpt <- person.getPropOptTyped(EmailAddressProp);
          email <- emailPropOpt.firstOpt;
          rc <- mainContext.requestOpt;
          // This is the value that we're going to use to identify this person when they
          // click on the link. We need to include the email address, to guard against it
          // being changed in the Person record after the email is sent. (Which could be
          // used as a spam vector, I suspect.)
          idString = person.id.toString + ":" + email.addr;
          signed = Hasher.sign(idString, emailSepChar);
          encoded = encodeURL(signed.toString);
          // TODO: this surely belongs in a utility somewhere -- it constructs the full path to a Thing, plus some paths.
	      // Technically speaking, we are converting a Link to an ExternalLink, then adding params.
	      url = urlBase + "u/" + rc.ownerHandle + "/" + state.toThingId + "/" + 
	        "?" + inviteParam + "=" + encoded
        }
        yield HtmlUI.HtmlValue(s"""<b><a href="$url">Click here</a></b> to accept the invitation.""")
        
      inviteOpt.getOrElse(QL.WarningValue("This appears to be an incorrect use of _spaceInvitation."))
    }
  }
  
  case class Invitee(email:EmailAddress, display:String)
  
  /**
   * Invite some people to join this Space. rc.state must be established (and authentication dealt with) before
   * we get here.
   * 
   * TODO: *boy*, this is mapping a lot of Futures together. Can we rewrite this as a clean for comprehension over
   * Futures, with some subroutines?
   */
  def inviteMembers(rc:RequestContext, inviteeEmails:Seq[EmailAddress], collaboratorIds:Seq[OID], originalState:SpaceState):Future[InvitationResult] = {
    // TODO: this is much too arbitrary:
    implicit val timeout = Timeout(30 seconds)
    
    val inviteeRole = {
	  val currentDefaultOpt = for {
	    rolesPV <- originalState.getPropOpt(AccessControl.PersonRolesProp)(originalState)
	    roleId <- rolesPV.firstOpt
	  }
	    yield roleId
	      
	  currentDefaultOpt.getOrElse(Roles.BasicMemberRole.id)
    }
    
    // Filter out any of these email addresses that have already been invited:
    val currentMembers = originalState.descendants(PersonOID, false, true)
    // This is a Map[address,Person] of all the members of this Space:
    // TODO: make this smarter! Should work for *all* addresses of the members, not just the ones in the Person
    // records! Sadly, that probably means a DB lookup.
    // TODO: the use of toLowerCase in this method is an abstraction break, as is the fact that we're digging
    // into EmailAddress.addr. Instead, the Email PType should provide enough machinery to be able to do this
    // properly, by extending matches() with some proper comparators.
    val currentEmails = currentMembers.
      map(member => (member.getPropOptTyped(EmailAddressProp)(originalState) -> member)).
      filter(pair => (pair._1.isDefined && !pair._1.get.isEmpty)).
      map(entry => (entry._1.get.first.addr.toLowerCase(), entry._2)).
      toMap
    val (existingEmails, explicitEmails) = inviteeEmails.partition(newEmail => currentEmails.contains(newEmail.addr.toLowerCase()))
    
    // TODO: merge this clause with the above one, instead of running through the whole list twice. Maybe rethink the
    // whole bloody approach, which is fairly ancient. In general, note that we are doing a lot of duplication of fairly
    // similar processes, one with email addresses and one with OIDs; there is almost certainly a higher-level function
    // or three crying to get factored out here.
    val currentCollabs = currentMembers.
      map(member => (member.getPropOptTyped(IdentityLink)(originalState) -> member)).
      filter(pair => (pair._1.isDefined && !pair._1.get.isEmpty)).
      map(entry => (entry._1.get.first, entry._2)).
      toMap
    val (existingCollabs, newCollabs) = collaboratorIds.partition(collabId => currentCollabs.contains(collabId))
    
    val existingPeople = 
      existingEmails.flatMap(email => currentEmails.get(email.addr.toLowerCase())) ++
      existingCollabs.flatMap(id => currentCollabs.get(id))
      
    val emailInvitees = explicitEmails.map { email => 
      val prefix = email.addr.takeWhile(_ != '@')
      val displayName = "Invitee " + prefix

      Invitee(email, displayName) 
    }

    val emailFut:Future[Seq[Invitee]] =
      if (collaboratorIds.length > 0) {
        // If we're inviting Collaborators, we need to go ask the Identity Cache for their info, to find out their email
        // addresses:
        IdentityAccess.getFullIdentities(collaboratorIds).map { found =>
          val collabIdentities = found.values
          val collabInvitees = collabIdentities.map { identity =>
            Invitee(identity.email, identity.name)
          }
          collabInvitees.toSeq ++ emailInvitees
        }
      } else {
        // We're just doing invites by email, so this Future becomes a no-op:
        Future.successful(emailInvitees)      
      }
    
    emailFut.flatMap { invitees =>
	    // Create Person records for all the new ones:
	    // We need to do this as a fairly complex fold, instead of simply blasting out the asks in parallel, to
	    // make sure that we end correctly with the *last* version of the state:
	    // TODO: add a CreateThings message that allows me to do multi-create, so we can avoid this complexity:
	    val start = Future.successful((originalState, Seq.empty[Thing]))
	    val futs = (start /: invitees) { (fut, invitee) =>
	      fut.flatMap { case (state, people) =>
	        // TODO: this really shouldn't hold the email address, since that is privileged information:
	        val propMap = 
	          toProps(
	            EmailAddressProp(invitee.email.addr),
	            DisplayNameProp(invitee.display),
	            AccessControl.PersonRolesProp(inviteeRole),
	            AccessControl.CanReadProp(AccessControl.OwnerTag))()
	        val msg = CreateThing(rc.requester.get, state.id, Kind.Thing, PersonOID, propMap)
	        val nextFuture = SpaceOps.spaceRegion ? msg
          // TODO: this code is fundamentally suspicious. It *probably* doesn't actually send SpaceState
          // cross-node, but it comes closer than I like:
	        nextFuture.mapTo[ThingFound].map { case ThingFound(id, newState) =>
	          (newState, people :+ newState.anything(id).get)
	        }        
	      }
	    }
	    
	    futs.map { case (updatedState, people) =>
	      implicit val finalState = updatedState
	      val context = updatedState.thisAsContext(rc, finalState, ecology)
	      val subjectQL = QLText(rc.ownerName + " has invited you to join the Space " + updatedState.displayName)
	      val inviteLink = QLText("""
	        |
	        |------
	        |
	        |[[_spaceInvitation]]""".stripMargin)
	      val bodyQL = updatedState.getPropOpt(InviteText).flatMap(_.firstOpt).getOrElse(QLText("")) + inviteLink
	      // TODO: we probably should test that sentTo includes everyone we expected:
	      // TODO: this shouldn't be explicitly email. Instead, this should be sending a Notification, which goes
	      // to email under certain circumstances:
	      val sentTo = Email.sendToPeople(context, people ++ existingPeople, subjectQL, bodyQL)
	    
	      val existingIds = existingPeople.map(_.id).toSet
	      val newPeople = people.filterNot(p => existingIds.contains(p.id))
	      InvitationResult(newPeople.map(_.displayName), existingPeople.map(_.displayName))  
	    }          
    }

  }
  
  /**
   * This is called via callbacks when we are beginning to render a page. It looks to see whether the
   * URL is an invitation to join this Space, and goes to the Invitation workflow if so.
   * 
   * TODO: this is dependent on PlayRequestContext, which means that it really belongs in controllers!
   */
  object InviteLoginChecker extends Contributor[PlayRequestContext,PlayRequestContext] {
    def notify(rc:PlayRequestContext, sender:Publisher[PlayRequestContext, PlayRequestContext]):PlayRequestContext = {
      val rcOpt =
        for (
          encodedInvite <- rc.firstQueryParam(inviteParam);
          spaceId <- rc.spaceIdOpt;
          ownerHandle <- rc.reqOwnerHandle;
          hash = SignedHash(encodedInvite, emailSepChar);
          // TODO: we should do something smarter if this fails:
          if (Hasher.checkSignature(hash));
          SignedHash(_, _, msg, _) = hash;
          Array(personIdStr, emailAddrStr, _*) = msg.split(":");
          emailAddr = EmailAddress(emailAddrStr);
          updates = Map((personParam -> personIdStr), (identityEmail -> emailAddrStr))
        )
          yield rc.copy(sessionUpdates = rc.sessionUpdates ++ rc.returnToHereUpdate ++ updates,
              redirectTo = Some(controllers.routes.LoginController.handleInvite(ownerHandle, spaceId)))
              
      // This gets picked up in Application.withSpace(), and redirected as necessary.
      rcOpt.getOrElse(rc)
    }
  }
  
  /**
   * This checks all the preconditions, and sends a request off to the SpaceManager to attach the
   * local Person record to the Identity. If it all succeeds, this will eventually produce a ThingFound(personId, state).
   * 
   * TODO: this depends on Play, so it should be in controllers!
   */
  def acceptInvitation[B](rcRaw:RequestContext)(cb:ThingResponse => Future[B])(implicit state:SpaceState):Option[Future[B]] = {
    val rc = rcRaw.asInstanceOf[PlayRequestContext]
    for (
      personIdStr <- rc.sessionCookie(personParam);
      personId = OID(personIdStr);
      person <- state.anything(personId);
      user <- rc.requester;
      // TODO: currently, we're just taking the first identity, arbitrarily. But in the long run, I should be able
      // to choose which of my identities is joining this Space:
      identity <- user.identityBy(_ => true);
      membershipResult = UserAccess.addSpaceMembership(identity.id, state.id);
      changeRequest = ChangeProps(SystemUser, state.id, person.toThingId, 
          toProps(
            IdentityLink(identity.id),
            DisplayNameProp(identity.name))())
    )
      yield SpaceOps.askSpace[ThingResponse, B](changeRequest)(cb)
  }
  
  def getPersonIdentity(person:Thing)(implicit state:SpaceState):Option[OID] = {
    withCache(_.localPerson(person.id).map(_.id))
  }
  
  def hasPerson(user:User, personId:OID)(implicit state:SpaceState):Boolean = {
    withCache { cache =>
      user.identities.exists(identity => isPerson(identity.id, personId))
    }
  }
  
  def isPerson(identityId:OID, personId:OID)(implicit state:SpaceState):Boolean = {
    withCache(_.localPerson(identityId).map(_.id == personId).getOrElse(false))
  }
  
  def isPerson(identity:IdentityId, person:Thing)(implicit state:SpaceState):Boolean = {
    isPerson(identity, person.id)
  }
  
  def isPerson(identity:Identity, person:Thing)(implicit state:SpaceState):Boolean = {
    isPerson(identity.id, person)
  }
  
  def hasMember(identity:IdentityId)(implicit state:SpaceState):Boolean = {
    withCache(_.localPerson(identity).map(_.hasProp(IdentityLink)).getOrElse(false))
  }
  
  def localIdentities(user:User)(implicit state:SpaceState):Iterable[Identity] = {
    withCache { cache =>
      user.identities.filter(identityId => cache.hasPerson(identityId.id))
    }
  }

  def localPerson(identity:Identity)(implicit state:SpaceState):Option[Thing] = {
    localPerson(identity.id)
  }

  def localPerson(identity:IdentityId)(implicit state:SpaceState):Option[Thing] = {
    withCache(_.localPerson(identity))
  }
  
  def localPerson(user:User)(implicit state:SpaceState):Option[Thing] = {
    withCache { cache =>
      user.identities.map(localPerson(_)).flatten.headOption
    }
  }
}
