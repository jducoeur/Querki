package querki.email

import scala.util.{Failure, Success, Try}

import models.{PropertyBundle, PTypeBuilder, ThingState, Wikitext}

import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.identity.{Identity, SystemUser}
import querki.values.{ElemValue, QLContext}

/**
 * Opaque wrapper for the concept of a "session"; the implementation depends on which version
 * of the EmailSender we're using.
 */
private [email] trait EmailSession

/**
 * This private interface for the email system gets implemented by either the RealEmailSender
 * (most of the time), or the TestEmailSender (during tests).
 */
private [email] trait EmailSender extends EcologyInterface {
  /**
   * The details of the session are intentionally hidden, since they are completely different
   * depending on the EmailSender.
   */
  type TSession <: EmailSession

  /**
   * This lets us create one Session, and send a bunch of emails during it, for efficiency.
   */
  def createSession():TSession
  def sendInternal(session:TSession, from:String, 
      recipientEmail:EmailAddress, recipientName:String, requester:Identity, 
      subject:Wikitext, bodyMain:Wikitext):Try[Unit]
}

class EmailModule(e:Ecology) extends QuerkiEcot(e) with Email with querki.core.MethodDefs {

  import querki.email.MOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val Links = initRequires[querki.links.Links]
  
  lazy val EmailSender = interface[EmailSender]
  lazy val QL = interface[querki.ql.QL]
    
  lazy val DeprecatedProp = Basic.DeprecatedProp
  lazy val DisplayTextProp = Basic.DisplayTextProp
  lazy val InternalProp = Core.InternalProp
  lazy val QLType = Basic.QLType
  
  def fullKey(key:String) = "querki.mail." + key
  lazy val from = Config.getString(fullKey("from"))
  lazy val test = Config.getBoolean(fullKey("test"), false)
  
  // Create the appropriate Email Sender, depending on whether we are in test mode or not. Note
  // that this is a sub-Ecot; we actually fetch it by the EmailSender interface later, rather than
  // holding on to the concrete type, to make sure we keep the interfaces clean.
  if (test) {
    new TestEmailSender(ecology)
  } else {
    new RealEmailSender(ecology)
  }
  
  def sendSystemEmail(recipient:Identity, subject:Wikitext, body:Wikitext):Try[Unit] = {
    val session = EmailSender.createSession()
    
    EmailSender.sendInternal(session, from, recipient.email, recipient.name, SystemUser.mainIdentity, subject, body)
  }
  
  /******************************************
   * TYPES
   ******************************************/
  
  class EmailAddressType(tid:OID) extends SystemType[EmailAddress](tid,
      toProps(
        setName("_Email Address Type"),
        Core.InternalProp(true),
        Summary("Represents an email address"),
        Details("""This Type represents an email address.
            |
            |_Email Address Type is, quite intentionally, marked System Hidden. That is, values of
            |_Email Address Type are scrubbed out before end users get to see them; this is
            |important for privacy.
            |
            |You should not try to use this type yourself. We might add a user-visible Email Address Type
            |sometime later, but you should never use this internal one.""".stripMargin)
      )) with PTypeBuilder[EmailAddress,String]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = EmailAddress(v)
    def doSerialize(v:EmailAddress)(implicit state:SpaceState) = v.addr
    // TODO: in the long run, this probably should render as a clickable URL?
    def doWikify(context:QLContext)(v:EmailAddress, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
      Future.successful(Wikitext(v.addr))
    
    def doDefault(implicit state:SpaceState) = EmailAddress("")
    def wrap(raw:String):valType = EmailAddress(raw)
    
    override def doMatches(left:EmailAddress, right:EmailAddress):Boolean = left.addr.equalsIgnoreCase(right.addr)
    
    def doComputeMemSize(v:EmailAddress):Int = v.addr.length
  }
  lazy val EmailAddressType = new EmailAddressType(EmailTypeOID)
  override lazy val types = Seq(EmailAddressType)
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val showSendEmail = new SystemProperty(EmailShowSendOID, TextType, ExactlyOne,
      toProps(
        setName("Email Results"),
        InternalProp(true),
        DeprecatedProp(true),
        Summary("Internal property, used in the process of sending email. Do not mess with this!")
      ))
  
  lazy val EmailAddressProp = new SystemProperty(EmailPropOID, EmailAddressType, Optional,
      toProps(
        setName("_Email Address"),
        InternalProp(true),
        Basic.SystemHiddenProp(true),
        DeprecatedProp(true),
        Summary("An email address for a Person"),
        Details("""This Property represents an email address.
            |
            |_Email Address is, quite intentionally, marked System Hidden. That is, values of
            |_Email Address are scrubbed out before end users get to see them; this is
            |important for privacy.
            |
            |You should not try to use this property yourself. We might add a user-visible Email Address Type
            |sometime later, but you should never use this internal one.
            |
            |This Probably will probably be eliminated entirely in the medium term.""".stripMargin)))
  
  lazy val emailTo = new SystemProperty(EmailToOID, LinkType, QSet,
        toProps(
          setName("Email To"),
          InternalProp(true),
          DeprecatedProp(true),
          Links.LinkModelProp(querki.identity.MOIDs.PersonOID),
          Summary("Who should this email be sent to?"),
          Details("""This is the raw list of people to send this email to. It should point to one or more
              |Person Things, each of which should have an Email Address set.
              |
              |If you want to do something fancier than sending to specific people, see the Recipients property.""".stripMargin)))

  lazy val emailSubject = new SystemProperty(EmailSubjectOID, TextType, ExactlyOne,
      toProps(
        setName("Email Subject"),
        InternalProp(true),
        DeprecatedProp(true),
        Summary("The title of the email")))

  lazy val emailBody = new SystemProperty(EmailBodyOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Email Body"),
        InternalProp(true),
        DeprecatedProp(true),
        Summary("The Contents of the email"),
        Details("""The contents of the email may contain more or less arbitrary wikitext; these will be
            |rendered in the HTML version of the email pretty much the same as they would be in the browser.
            |The email will also contain the raw wikitext, as the "plaintext" version.""".stripMargin)))
  
  lazy val sentToProp = new SystemProperty(SentToOID, LinkType, QSet,
      toProps(
        setName("Sent To"),
        InternalProp(true),
        DeprecatedProp(true),
        Summary("The Persons that this mail has already been sent to."),
        Details("""This Property is set automatically when the email is sent. You usually should not modify
            |it by hand, but it is sometimes useful to do so before sending or resending the email, since the
            |email will *not* be sent to anyone in this set.""".stripMargin)))
  
  lazy val recipientsProp = new SystemProperty(RecipientsOID, QLType, ExactlyOne,
      toProps(
        setName("Recipients"),
        DeprecatedProp(true),
        Summary("Who will this email be sent to?"),
        Details("""The Recipients property declares who will receive this email. It is a QL expression, and
            |you should only modify it if you know what you are doing. By default, it simply defers to
            |the contents of the Email To property -- for ordinary email, you should just list the people
            |who are to receive this in Email To. But you can edit Recipients to be any other QL expression,
            |which can make some problems much easier. For example, if you want to send the email to
            |every Person listed in this Space, set Recipients to "Person._instances".
            |
            |The QL expression given in here must produce a List of Links to Persons.""".stripMargin)))

  override lazy val props = Seq(
    // The actual email-address property
    EmailAddressProp,
    
    // TODO: introduce the Recipients property. This is an indirection between
    // Email Message and Email To, a QL expression that returns the list of people
    // people to email.
    
    emailTo,
    
    emailSubject,
    
    emailBody,
    
    showSendEmail,
    
    sentToProp,
    
    recipientsProp
  )
  
  /***********************************************
   * THINGS
   ***********************************************/
    
  override lazy val things = Seq(
    ThingState(EmailTemplateOID, systemOID, RootOID,
      toProps(
        setName("Email Message"),
        Core.IsModelProp(true),
        emailTo(),
        emailSubject(""),
        emailBody(),
        recipientsProp("Email To"),
        showSendEmail("""Email successfully sent to:
[[Send Email -> ""* ____ - [[Email Address]]""]]
"""),
        DisplayTextProp("""
[[_formLine(""**Subject**:"", Email Subject._edit)]]

[[_formLine(""**Recipients**:"", _code(Recipients))]]

[[_formLine(""**Explicitly To**:"", Email To._editOrElse(Email To -> _commas))]]
            
[[_formLine(""**Already Sent To**:"", Sent To._editOrElse(Sent To -> _commas))]]
            
[[_formLine(""**Body**:"", Email Body._edit)]]
    
------
            
**[Click here to send this email](?prop=Email+Results)**
""")))
  )
  
  /***********************************************
   * METHOD CONTENTS
   ***********************************************/
    
  lazy val AccessControl = interface[querki.security.AccessControl]
  
  def sendToPeople(context:QLContext, people:Seq[Thing], subjectQL:QLText, bodyQL:QLText)(implicit state:SpaceState):Future[Seq[OID]] = {
    val session = EmailSender.createSession()
    
    val oidOptFuts = people map { person =>
      sendToPerson(context, person, session, subjectQL, bodyQL, from)
    }
    Future.sequence(oidOptFuts) map (_.flatten)
  }
  
  def sendRaw(recipientEmail:EmailAddress, recipientName:String, subject:Wikitext, body:Wikitext, from:String, requester:Identity):Future[Unit] = {
    val session = EmailSender.createSession()
    // All of this email-sending stuff is blocking, so it *should* be creating Futures. Let's at least start
    // doing that:
    EmailSender.sendInternal(session, from, recipientEmail, recipientName, requester, subject, body) match {
      case Success(_) => fut(())
      case Failure(err) => Future.failed(err)
    }
  }
	
  def sendToPerson(context:QLContext, person:Thing, session:EmailSender.TSession, subjectQL:QLText, bodyQL:QLText, from:String)(implicit state:SpaceState):Future[Option[OID]] = {
    val name = person.displayName
    val addrList = person.getProp(EmailAddressProp)
    if (addrList.isEmpty)
      Future.successful(None)
    else {
      val addr = addrList.first
	      	    
      // TODO: this originally derived from the higher-level context of the Email itself. Really, it should do so.
      // But PersonModule needs to get at this Person object from the Context *somehow*, and for now it's easiest
      // to do so as context.root.
      // The right solution is probably to predefine a name binding, which gets passed into the new QLContext, and
      // use that in PersonModule. But first we need to introduce the idea of name bindings!
      // Once that is done, restore the incoming context as the parent of this one.
      val personContext = QLContext(ExactlyOne(ElemValue(person.id, LinkType)), context.requestOpt, None) //Some(context))
    
      for {
        subject <- QL.process(subjectQL, personContext.forProperty(emailSubject))
        body <- QL.process(bodyQL, personContext.forProperty(emailBody))
      
        // Note that emails are sent with the ReplyTo set to whoever asked for this email to be generated.
        // You are responsible for your own emails.
        // TODO: This will need to get more intelligent about the identity to use for the ReplyTo: it should
        // use the requester's Identity that is being used for *this* Space. Right now, we're potentially
        // leaking the wrong email address. But it'll do to start.
        result = EmailSender.sendInternal(session, from, addr, name, context.request.requesterOrAnon.mainIdentity, subject, body)

      }
        yield result match {
          case Success(_) => Some(person.id)
          case Failure(ex) => QLog.error("Got an error while sending email ", ex); None 
        }
    }
  }
}
