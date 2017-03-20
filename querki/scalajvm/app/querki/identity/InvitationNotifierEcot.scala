package querki.identity

import models._
import querki.ecology._
import querki.email._
import querki.globals._
import querki.notifications._
import querki.core.QLText
import querki.persistence._
import querki.time.DateTime
import querki.util.{Hasher, SafeUrl, SignedHash}
import querki.values.{QLContext, SpaceState}

import EmailFunctions._

private [identity] object InvitationNotifierMOIDs extends EcotIds(66) {
  val InvitationSenderOID = moid(1)
  val InvitationTextOID = moid(2)
  val InvitationURLOID = moid(3)
  val InvitationSenderNameOID = moid(4)
  val InvitationSpaceNameOID = moid(5)
  val InvitationSpaceOwnerOID = moid(6)
  
  val UnsubSenderOID = moid(7)
  val UnsubAllInvitesOID = moid(8)
}

case class DHUnsubSenderInvites(
  @KryoTag(1) sender:OID
) extends UnsubEvent with UseKryo

case class DHUnsubAllInvites() extends UnsubEvent with UseKryo

class InvitationNotifierEcot(e:Ecology) extends QuerkiEcot(e) with Notifier with EmailNotifier with NotifyInvitations {
  import InvitationNotifierMOIDs._ 
  
  val Basic = initRequires[querki.basic.Basic]
  
  lazy val Email = interface[querki.email.Email]
  lazy val IdentityAccess = interface[IdentityAccess]
  lazy val Notifications = interface[querki.notifications.Notifications]
  lazy val NotifierRegistry = interface[querki.notifications.NotifierRegistry]
  lazy val Person = interface[Person]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val System = interface[querki.system.System]
  lazy val Unsubscribe = interface[querki.email.Unsubscribe]
  
  lazy val PlainTextType = Basic.PlainTextType 
  lazy val SystemState = System.State
  
  lazy val urlBase = Config.getString("querki.app.urlRoot")
  
  override def postInit() = {
    NotifierRegistry.register(this)
  }
  
  override def term() = {
    NotifierRegistry.unregister(this)
  }
  
  override def persistentMessages = persist(66,
    (classOf[DHUnsubSenderInvites] -> 100),
    (classOf[DHUnsubAllInvites] -> 101)
  )
  
  object Notifiers {
    val InvitationNotifierId:Short = 1
  }
  
  ////////////////////////////////////
  //
  // Implementation of Notifier
  //
  ////////////////////////////////////
  
  def id = NotifierId(InvitationNotifierMOIDs.ecotId, Notifiers.InvitationNotifierId)

  def summarizeAt:SummarizeAt.SummarizeAt = SummarizeAt.None
  
  def summarizeNew(context:QLContext, notes:Seq[Notification]):Future[SummarizedNotifications] = {
    if (notes.length != 1)
      throw new Exception("InvitationNotifier.summarizeNew current expects exactly one notification at a time!")
      
    val note = notes.head
    render(context, note) map { rendered =>
      SummarizedNotifications(rendered.headline, rendered.content, notes)      
    }
  }
   
  /**
   * Actually send out an Invitation.
   * 
   * Note that this requires the SpaceState *after* all of the Invitees have been added as Persons.
   * 
   * This is kind of incestuous with Person.inviteMembers(), and it isn't 100% obvious what belongs where.
   */
  def notifyInvitation(req:User, textOpt:Option[QLText], invitees:Seq[FullIdentity])(implicit state:SpaceState):Unit = {
    val sender = req.mainIdentity
    
    Person.withCache { cache =>
      // Invitations are individualized, because the invite link is distinct for each one. (At least for now -- someday,
      // we might allow a "public invitation" to a Space, but there are real abuse problems there. I don't think that's
      // the same thing as what we mean by "invitation".)
      invitees.foreach { invitee =>
        // Note that we have to dig directly into the cache to get at this Person, since they presumably are not
        // an accepted Member:
        val person = cache.allPeopleByIdentityId.get(invitee.id).getOrElse(throw new Exception(s"Space ${state.id} doesn't contain a Person record for Identity $invitee!"))
        val url = generateInviteLink(person, invitee.id, invitee.email, state)
        val payload = toProps(
          InvitationSender(sender.id),
          InvitationURL(url),
          InvitationSenderName(sender.name),
          InvitationSpaceName(state.displayName),
          InvitationSpaceOwner(state.owner)
        ) ++ textOpt.map { text => toProps(InvitationText(text.text)) }.getOrElse(emptyProps)
        
        // IMPORTANT TODO: we're currently generating notifications as already Read, because there is no way for the
        // user to mark them as Read yet. Once we have the needed UI, change the parameter below.
        val note = Notification(
          EmptyNotificationId,
          sender.id, 
          None,
          id,
          DateTime.now,
          Some(state.id), 
          None, 
          SpacePersistence.serProps(payload, state),
          true,
          false)
          
        Notifications.send(req, ExplicitRecipients(invitees.map(_.id)), note)
      }
    }
  }
  
  val inviteParam = "invite"
  
  def makeInviteLink(encoded:String, state:SpaceState):String = {
    // TODO: this surely belongs in a utility somewhere -- it constructs the full path to a Thing, plus some paths.
    // Technically speaking, we are converting a Link to an ExternalLink, then adding params.
    urlBase + 
      "u/" + state.ownerHandle + 
      "/" + state.toThingId + "/" + 
      "#_handleInvite" +
      "?" + inviteParam + "=" + encoded    
  }
  
  def generateInviteLink(person:Thing, inviteeId:IdentityId, email:EmailAddress, state:SpaceState):String = {
    val idString = person.id.toString + ":" + email.addr + ":" + inviteeId.toString
    val signed = Hasher.sign(idString, emailSepChar)
    val encoded = SafeUrl(signed.toString)
    makeInviteLink(encoded, state)
  }
  
  val sharePrefix = "share;"
  
  def generateShareableLink(roleId:OID, state:SpaceState):String = {
    val idString = roleId.toString
    val signed = Hasher.sign(idString, emailSepChar)
    val encoded = SafeUrl(sharePrefix + signed.toString)
    makeInviteLink(encoded, state)
  }
  
  def parseInvite(encodedInvite:String):Option[ParsedInvitation] = {
    val (actualInvite, isOpen) =
      if (encodedInvite.startsWith(sharePrefix))
        (encodedInvite.drop(sharePrefix.length), true)
      else
        (encodedInvite, false)
    val hash = SignedHash(actualInvite, emailSepChar)
    if (!Hasher.checkSignature(hash))
      None
    else {
      val SignedHash(_, _, msg, _) = hash
      if (isOpen) {
        Some(OpenInvitation(OID(msg)))
      } else {
        val Array(personIdStr, emailAddrStr, identityIdStr, _*) = msg.split(":")
        Some(
          SpecificInvitation(
            OID(personIdStr), 
            IdentityAccess.makeGuest(identityIdStr, emailAddrStr)))
      }
    }
  }
  
  case class InvitePayload(senderId:IdentityId, textQVOpt:Option[QValue], inviteUrl:String, senderName:String, spaceName:String, spaceOwner:OID)
  def parsePayload(note:Notification):InvitePayload = {
    val rawPayload = note.payload
    val payload = SpacePersistence.deserProps(rawPayload, SystemState)
    
    val senderId = payload.getFirst(InvitationSender)
    val textQVOpt = payload.get(InvitationText)
    val url = payload.getFirst(InvitationURL).text
    val senderName = payload.getFirst(InvitationSenderName).text
    val spaceName = payload.getFirst(InvitationSpaceName).text
    val spaceOwner = payload.getFirst(InvitationSpaceOwner)
    
    InvitePayload(senderId, textQVOpt, url, senderName, spaceName, spaceOwner)
  }
  
  def render(context:QLContext, note:Notification):Future[RenderedNotification] = {
    val InvitePayload(senderId, textQVOpt, url, senderName, spaceName, spaceOwner) = parsePayload(note)
    
    for {
      body <- textQVOpt.map(_.wikify(context)).getOrElse(Future.successful(Wikitext("")))
      headline = Wikitext(s"""$senderName has invited you to join $spaceName!""")
      joinButton = HtmlWikitext(s"""<a href="$url" class="btn btn-primary">Join Space $spaceName</a>""".stripMargin)
    }
      yield RenderedNotification(headline, body + Wikitext("\n\n") + joinButton)
  }
  
  def emailNotifier:Option[EmailNotifier] = Some(this)
  
  ////////////////////////////////////
  //
  // Implementation of EmailNotifier
  //
  ////////////////////////////////////
  
  def shouldSendEmail(note:Notification, unsubs:List[UnsubEvent]):Boolean = {
    def violation(unsub:UnsubEvent):Boolean = {
      unsub match {
        case DHUnsubAllInvites() => true
        case DHUnsubSenderInvites(sender) => note.sender == sender
        case _ => throw new Exception(s"InvitationNotifierEcot.shouldSendEmail() got unexpected UnsubEvent $unsub")
      }
    }
    
    !unsubs.exists(violation)
  }
  
  val wikibreak = Wikitext("\n\n")
  
  def toEmail(note:Notification, recipient:FullIdentity):Future[EmailMsg] = {
    val Notification(id, sender, toIdentityIdOpt, _, sentTime, spaceIdOpt, _, _, _, _) = note
    val InvitePayload(senderId, textQVOpt, url, senderName, spaceName, spaceOwner) = parsePayload(note)
    val spaceId = spaceIdOpt.getOrElse(throw new Exception(s"Somehow got InvitationNotification with no spaceId: $note"))
    
    val hasBody = textQVOpt.isDefined

    for {
      body <- textQVOpt.map(_.wikify(querki.values.EmptyContext(ecology))).getOrElse(Future.successful(Wikitext("")))
      subject = Wikitext(s"""$senderName has invited you to join $spaceName!""")
      spaceUrl = urlBase + "u/" + spaceOwner.toThingId + "/" + spaceId.toThingId + "/"
      headline = Wikitext(s"""$senderName has invited you to join [$spaceName]($spaceUrl)!""")
      hr = 
        if (hasBody) 
          HtmlWikitext("<hr/>") 
        else 
          Wikitext("")
      instructions =
        if (recipient.kind == IdentityKind.SimpleEmail)
          Wikitext(s"""Clicking on this link will let you set up a Querki login (for free) and begin using '$spaceName'.""".stripMargin)
        else
          Wikitext("")
      onlyYou = Wikitext("This link is intended for you personally; please do not share it with others.")
      joinButton = HtmlWikitext(s"""<div class="bottomlinkdiv"><a href="$url" class="btn btn-primary">Join Space '$spaceName'</a></div>""".stripMargin)
      fullBody =
        Wikitext("{{title:") + wikibreak + headline + wikibreak + Wikitext("}}") + wikibreak +
        body + wikibreak +
        hr + wikibreak +
        instructions + wikibreak +
        onlyYou + wikibreak +
        joinButton
      unsubLink = Unsubscribe.generateUnsubLink(this, recipient.id, recipient.email, spaceId.toString, senderId.toString, spaceName)
    }
      yield EmailMsg(
        EmailAddress(Email.from),
        recipient.email,
        recipient.name,
        senderName,
        subject,
        fullBody,
        Wikitext(s"""You received this email because $senderName invited you. 
          |If you don't want to receive these invitations, just click [Unsubscribe]($unsubLink).""".stripMargin)
      )
  }
  
  def unsubOptions(unsubInfo:UnsubInfo):Future[(Wikitext, Seq[UnsubOption])] = {
    val spaceIdStr :: senderIdStr :: spaceName :: Nil = unsubInfo.rest
    val senderId = OID(senderIdStr)
    
    IdentityAccess.getIdentity(senderId).map { senderOpt =>
      val sender = senderOpt.getOrElse(throw new Exception(s"Somehow got Invitation Unsub from unknown sender $senderId!"))
      (
        Wikitext(s"""**${sender.name}** invited you to join the Querki Space *$spaceName*. 
                    |[Click here for more information about Querki.](https://www.querki.net/help/#!What-is-Querki) 
                    |
                    |Use the buttons below to keep ${sender.name} from inviting you to this or any other Space,
                    |or to suppress Querki invitations entirely.""".stripMargin),
        
        Seq(
          UnsubOption(
            id.toString,
            UnsubSenderOID.toTOID,
            Some(senderIdStr),
            s"Block invitations from ${sender.name}",
            s"""Pressing this button will prevent ${sender.name} from sending you any more invitations to join
               |Spaces in Querki, but you will still be able to get invitations from other people.""".stripMargin
          ),
          
          UnsubOption(
            id.toString,
            UnsubAllInvitesOID.toTOID,
            None,
            s"Block all invitations to join Querki Spaces",
            s"""This will prevent anyone from inviting you to join their Spaces, permanently.""".stripMargin
          )
        )
      )
    }
  }
  
  def getUnsubEvent(unsubId:OID, context:Option[String]):(Wikitext, UnsubEvent with UseKryo) = {
    unsubId match {
      case UnsubSenderOID => {
        val sender = context.map(OID(_)).getOrElse(throw new Exception(s"Somehow got an UnsubSender request without the sender!"))
        (Wikitext("Saved -- they will no longer be able to send you invitations."), DHUnsubSenderInvites(sender))
      }
      case UnsubAllInvitesOID => 
        (Wikitext("Saved -- you will no longer receive invitations to Querki."), DHUnsubAllInvites())
    }
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val InvitationSender = new SystemProperty(InvitationSenderOID, LinkType, ExactlyOne,
    toProps(
      setName("_invitationNotifySender"),
      setInternal,
      Summary("Who sent this invitation, as an IdentityId")))
  
  lazy val InvitationText = new SystemProperty(InvitationTextOID, TextType, Optional,
    toProps(
      setName("_invitationNotifyText"),
      setInternal,
      Summary("The text of this invitation, if specified")))
  
  lazy val InvitationURL = new SystemProperty(InvitationURLOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_invitationURL"),
      setInternal,
      Summary("The actual link to click on to accept this invitation")))
  
  lazy val InvitationSenderName = new SystemProperty(InvitationSenderNameOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_invitationSenderName"),
      setInternal,
      Summary("The display name of the sender of this invitation")))
  
  lazy val InvitationSpaceName = new SystemProperty(InvitationSpaceNameOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_invitationSpaceName"),
      setInternal,
      Summary("The name of the Space this invitation is for")))
  
  lazy val InvitationSpaceOwner = new SystemProperty(InvitationSpaceOwnerOID, LinkType, ExactlyOne,
    toProps(
      setName("_invitationSpaceOwner"),
      setInternal,
      Summary("The Identity that owns the Space that is being invited to.")))
  
  override lazy val props = Seq(
    InvitationSender,
    InvitationText,
    InvitationURL,
    InvitationSenderName,
    InvitationSpaceName,
    InvitationSpaceOwner
  )
}
