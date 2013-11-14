package modules.email

import scala.util._

import javax.activation.DataHandler
import javax.mail._
import javax.mail.internet._
import javax.mail.util.ByteArrayDataSource
import com.sun.mail.smtp._

import models._
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import querki.identity._
import querki.values._

import modules.Modules._

import play.api.{Logger, Play}
import play.api.Play.current

/**
 * Represents an email address. For the moment this is basically just a String, but we'll gradually
 * add things like validation, so it's useful to get the abstraction clean now.
 */
case class EmailAddress(addr:String)

class EmailModule(val moduleId:Short) extends modules.Module {

  def fullKey(key:String) = "querki.mail." + key
  def getRequiredConf(key:String) = {
    val opt = Play.configuration.getString(fullKey(key))
    opt match {
      case Some(v) => v
      case None => throw new Exception("Didn't find required configuration key " + fullKey(key))
    }
  }
    
  lazy val smtpHost = getRequiredConf("smtpHost")
  lazy val from = getRequiredConf("from")
  lazy val debug = Play.configuration.getBoolean(fullKey("debug")).getOrElse(false)

  object MOIDs {
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
  import MOIDs._
  
  /**
   * The character that we use to separate strings involving email addresses. Chosen mostly
   * because it is not a legal character in email addresses.
   */
  val emailSepChar = ';'
  
  /******************************************
   * TYPES
   ******************************************/
  
  class EmailAddressType(tid:OID) extends SystemType[EmailAddress](tid,
      toProps(
        setName("Email")
      )) with PTypeBuilder[EmailAddress,String]
  {
    def doDeserialize(v:String) = EmailAddress(v)
    def doSerialize(v:EmailAddress) = v.addr
    // TODO: in the long run, this probably should render as a clickable URL?
    def doWikify(context:QLContext)(v:EmailAddress, displayOpt:Option[Wikitext] = None) = Wikitext(v.addr)
    
    val doDefault = EmailAddress("")
    def wrap(raw:String):valType = EmailAddress(raw)
    
    override def doMatches(left:EmailAddress, right:EmailAddress):Boolean = left.addr.equalsIgnoreCase(right.addr)
  }
  lazy val EmailAddressType = new EmailAddressType(EmailTypeOID)
  override lazy val types = Seq(EmailAddressType)
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  // The actual definition of this method is down below
  lazy val sendEmail = new SingleThingMethod(EmailSendOID, "Send Email", "Send this email message",
      """Invoke this method to actually send this email.
      |It will return a List of the Persons who the email was sent to this time.""".stripMargin, sendEmailIfAllowed)
  
  lazy val showSendEmail = new SystemProperty(EmailShowSendOID, TextType, ExactlyOne,
      toProps(
        setName("Email Results"),
        InternalProp(true),
        PropSummary("Internal property, used in the process of sending email. Do not mess with this!")
      ))
  
  lazy val emailAddress = new SystemProperty(EmailPropOID, EmailAddressType, Optional,
      toProps(
        setName("Email Address"),
        InternalProp(true),
        PropSummary("An email address for a Person"),
        PropDetails("""This Property represents the general notion of something that can have an email
            |address. It is available on Person, but you can reuse it wherever you like. In theory, you can
            |send email to anything that has an Email Address property.
            |
            |Note, however, that this Property is one optional address. If you want to require
            |that an address be given, or allow a list of them, you will need to create a
            |separate Property with the Email Address type.""".stripMargin)))
  
  lazy val emailTo = new SystemProperty(EmailToOID, LinkType, QSet,
        toProps(
          setName("Email To"),
          InternalProp(true),
          (LinkModelOID -> Optional(ElemValue(Person.MOIDs.PersonOID, LinkType))),
          PropSummary("Who should this email be sent to?"),
          PropDetails("""This is the raw list of people to send this email to. It should point to one or more
              |Person Things, each of which should have an Email Address set.
              |
              |If you want to do something fancier than sending to specific people, see the Recipients property.""".stripMargin)))

  lazy val emailSubject = new SystemProperty(EmailSubjectOID, TextType, ExactlyOne,
      toProps(
        setName("Email Subject"),
        InternalProp(true),
        PropSummary("The title of the email")))

  lazy val emailBody = new SystemProperty(EmailBodyOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Email Body"),
        InternalProp(true),
        PropSummary("The Contents of the email"),
        PropDetails("""The contents of the email may contain more or less arbitrary wikitext; these will be
            |rendered in the HTML version of the email pretty much the same as they would be in the browser.
            |The email will also contain the raw wikitext, as the "plaintext" version.""".stripMargin)))
  
  lazy val sentToProp = new SystemProperty(SentToOID, LinkType, QSet,
      toProps(
        setName("Sent To"),
        InternalProp(true),
        PropSummary("The Persons that this mail has already been sent to."),
        PropDetails("""This Property is set automatically when the email is sent. You usually should not modify
            |it by hand, but it is sometimes useful to do so before sending or resending the email, since the
            |email will *not* be sent to anyone in this set.""".stripMargin)))
  
  lazy val recipientsProp = new SystemProperty(RecipientsOID, QLType, ExactlyOne,
      toProps(
        setName("Recipients"),
        PropSummary("Who will this email be sent to?"),
        PropDetails("""The Recipients property declares who will receive this email. It is a QL expression, and
            |you should only modify it if you know what you are doing. By default, it simply defers to
            |the contents of the Email To property -- for ordinary email, you should just list the people
            |who are to receive this in Email To. But you can edit Recipients to be any other QL expression,
            |which can make some problems much easier. For example, if you want to send the email to
            |every Person listed in this Space, set Recipients to "Person._instances".
            |
            |The QL expression given in here must produce a List of Links to Persons.""".stripMargin)))

  override lazy val props = Seq(
    // The actual email-address property
    emailAddress,
    
    // TODO: introduce the Recipients property. This is an indirection between
    // Email Message and Email To, a QL expression that returns the list of people
    // people to email.
    
    emailTo,
    
    emailSubject,
    
    emailBody,
    
    sendEmail,
    
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
        IsModelProp(true),
        emailTo(),
        emailSubject(""),
        emailBody(),
        sendEmail(),
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
    
  import querki.access.AccessControl
  def sendEmailIfAllowed(t:Thing, context:QLContext) = {
    if (AccessControl.canEdit(context.state, context.request.requesterOrAnon, t.id))
      doSendEmail(t, context)
    else
      TextValue("You aren't allowed to send that email")
  }
  
  private def createSession():Session = {
    val props = System.getProperties()
    props.setProperty("mail.host", smtpHost)
    props.setProperty("mail.user", "querki")
    props.setProperty("mail.transport.protocol", "smtp")
    props.setProperty("mail.from", from)
    props.setProperty("mail.debug", "true")
	    
    val session = Session.getInstance(props, null)
	
    session.setDebug(debug)
    
    session
  }
  
  def sendToPeople(context:QLContext, people:Seq[Thing], subjectQL:QLText, bodyQL:QLText)(implicit state:SpaceState):Seq[OID] = {
    val session = createSession()
    
    people.flatMap { person =>
      sendToPerson(context, person, session, subjectQL, bodyQL, from)
    }
  }
  
  /**
   * The guts of actually sending email. Note that this can be invoked from a number of different circumstances,
   * some user-initiated and some not.
   */
  private def sendInternal(session:Session, from:String, 
      recipientEmail:EmailAddress, recipientName:String, requester:Identity, 
      subject:Wikitext, body:Wikitext):Try[Unit] = 
  Try {
	val msg = new MimeMessage(session)
	msg.setFrom(new InternetAddress(from))

	val replyAddrs = Array(new InternetAddress(requester.email.addr, requester.name).asInstanceOf[Address])
	msg.setReplyTo(replyAddrs)

    // TBD: there must be a better way to do this. It's a nasty workaround for the fact
	// that setRecipients() is invariant, I believe.
	val toAddrs = Array(new InternetAddress(recipientEmail.addr, recipientName).asInstanceOf[Address])	
	msg.setRecipients(Message.RecipientType.TO, toAddrs)
	
	msg.setSubject(subject.plaintext)
	
	// Attach the HTML...
	val bodyHtml = body.display
	val bodyPartHtml = new MimeBodyPart()
	bodyPartHtml.setDataHandler(new DataHandler(new ByteArrayDataSource(bodyHtml, "text/html")))      
	// ... and the plaintext...
	val bodyPlain = body.plaintext
	val bodyPartPlain = new MimeBodyPart()
	bodyPartPlain.setDataHandler(new DataHandler(new ByteArrayDataSource(bodyPlain, "text/plain")))
	// ... and set the body to the multipart:
	val multipart = new MimeMultipart("alternative")
	// IMPORTANT: these are in increasing order of importance, and Gmail will display the *last*
	// one by preference:
	multipart.addBodyPart(bodyPartPlain)
	multipart.addBodyPart(bodyPartHtml)
	msg.setContent(multipart)
	    
	msg.setHeader("X-Mailer", "Querki")
	msg.setSentDate(new java.util.Date())
	    
	Transport.send(msg)
  }
	
  def sendToPerson(context:QLContext, person:Thing, session:Session, subjectQL:QLText, bodyQL:QLText, from:String)(implicit state:SpaceState):Option[OID] = {
    val name = person.displayName
    val addrList = person.getProp(emailAddress)
    if (addrList.isEmpty)
      None
    else {
      val addr = addrList.first
	      	    
      // TODO: this originally derived from the higher-level context of the Email itself. Really, it should do so.
      // But PersonModule needs to get at this Person object from the Context *somehow*, and for now it's easiest
      // to do so as context.root.
      // The right solution is probably to predefine a name binding, which gets passed into the new QLContext, and
      // use that in PersonModule. But first we need to introduce the idea of name bindings!
      // Once that is done, restore the incoming context as the parent of this one.
      val personContext = QLContext(ExactlyOne(ElemValue(person.id, LinkType)), context.requestOpt, None) //Some(context))
    
      val subjectParser = new QLParser(subjectQL, personContext.forProperty(emailSubject))
      val subject = subjectParser.process	    
      val bodyParser = new QLParser(bodyQL, personContext.forProperty(emailBody))
      val body = bodyParser.process
      
      // Note that emails are sent with the ReplyTo set to whoever asked for this email to be generated.
      // You are responsible for your own emails.
      // TODO: This will need to get more intelligent about the identity to use for the ReplyTo: it should
      // use the requester's Identity that is being used for *this* Space. Right now, we're potentially
      // leaking the wrong email address. But it'll do to start.
      val result = sendInternal(session, from, addr, name, context.request.requesterOrAnon.mainIdentity, subject, body)
      result match {
        case Success(_) => Some(person.id)
        case Failure(ex) => Logger.error("Got an error while sending email ", ex); None 
      }
    }
  }
  
  def doSendEmail(t:Thing, context:QLContext) = {
    implicit val state = context.state
    val recipientsIndirect = t.getProp(recipientsProp)
    val previouslySentToOpt = t.getPropOpt(sentToProp)
    
    // Get the actual list of recipients:
    val recipientParser = new QLParser(recipientsIndirect.first, t.thisAsContext(context.request).forProperty(recipientsProp))
    val recipientContext = recipientParser.processMethod
    if (recipientContext.value.pType != LinkType) {
      ErrorValue("The Recipient property of an Email Message must return a collection of Links; instead, it produced " + recipientContext.value.pType.displayName)
    } else {
      val recipients = recipientContext.value
      
        val subjectQL = t.getProp(emailSubject).first
        val bodyQL = t.getProp(emailBody).first		      
	    
	    // Construct the email:
	    val props = System.getProperties()
	    props.setProperty("mail.host", smtpHost)
	    props.setProperty("mail.user", "querki")
	    props.setProperty("mail.transport.protocol", "smtp")
	    props.setProperty("mail.from", from)
	    props.setProperty("mail.debug", "true")
	    
	    val session = Session.getInstance(props, null)
	
	    session.setDebug(debug)

	    val sentTo = recipients.flatMap(LinkType) { personOID =>
	      if (previouslySentToOpt.isDefined && previouslySentToOpt.get.contains(personOID))
	        None
	      else {
	        val thing = state.anything(personOID)
	        thing match {
	          case Some(person) => sendToPerson(context, person, session, subjectQL, bodyQL, from)
	          case None => None  // TODO: some kind of error here?
	        }
	      }
	    }
	    val resultingList = QList.makePropValue(sentTo.map(ElemValue(_, LinkType)).toList, LinkType)
	    
	    val req = context.request
	    val fullSentTo = previouslySentToOpt match {
	      case Some(previouslySentTo) => previouslySentTo ++ sentTo
	      case None => resultingList
	    } 
	    val changeRequest = ChangeProps(req.requester.get, state.owner, state.id, t.toThingId, toProps(SentToOID -> fullSentTo)())
	    SpaceManager.ask(changeRequest) { resp:ThingResponse =>
	      resp match {
	        case ThingFound(id, state) => Logger.info("Noted email recipients")
	        // TODO: what should we do in case of failure?
	        case ThingFailed(error, msg, stateOpt) => Logger.error("Unable to record email recipients: " + msg)
	      }
	    }
	    
	    resultingList
    }
  }
}

object EmailModule {
  import modules.Modules.Email._
    
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
  def sendSystemEmail(recipient:Identity, subject:Wikitext, body:Wikitext):Try[Unit] = {
    val session = createSession()
    
    sendInternal(session, from, recipient.email, recipient.name, SystemUser.mainIdentity, subject, body)
  }
}