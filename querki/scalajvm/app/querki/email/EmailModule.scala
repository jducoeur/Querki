package querki.email.impl

import querki.email._

import scala.concurrent.Future
import scala.util._

import javax.activation.DataHandler
import javax.mail._
import javax.mail.internet._
import javax.mail.util.ByteArrayDataSource
import com.sun.mail.smtp._

import models._

import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.identity._
import querki.spaces.SpaceManager
import querki.spaces.messages.{ChangeProps, ThingError, ThingFound, ThingResponse}
import querki.values._

import play.api.{Logger, Play}
import play.api.Play.current

class EmailModule(e:Ecology) extends QuerkiEcot(e) with Email with querki.core.MethodDefs {

  import querki.email.MOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val Links = initRequires[querki.links.Links]
  
  lazy val QL = interface[querki.ql.QL]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  lazy val QLType = Basic.QLType
  
  lazy val DeprecatedProp = Basic.DeprecatedProp
  lazy val DisplayTextProp = Basic.DisplayTextProp
  lazy val InternalProp = Core.InternalProp
  
  def fullKey(key:String) = "querki.mail." + key
  def getRequiredConf(key:String) = {
    val opt = Play.configuration.getString(fullKey(key))
    opt match {
      case Some(v) => v
      case None => throw new Exception("Didn't find required configuration key " + fullKey(key))
    }
  }
  
  lazy val from = getRequiredConf("from")
  lazy val smtpHost = getRequiredConf("smtpHost")
  lazy val smtpPort = Config.getInt(fullKey("port"), 0)
  lazy val debug = Play.configuration.getBoolean(fullKey("debug")).getOrElse(false)
  lazy val username = Config.getString(fullKey("smtpUsername"), "")
  lazy val password = Config.getString(fullKey("smtpPassword"), "")
  
  private def createSession():Session = {
    val props = System.getProperties()
    props.setProperty("mail.host", smtpHost)
    if (smtpPort != 0) {
      props.setProperty("mail.smtp.port", smtpPort.toString)
    }
    props.setProperty("mail.user", "querki")
    props.setProperty("mail.transport.protocol", "smtp")
    props.setProperty("mail.from", from)
    props.setProperty("mail.debug", "true")
    if (username.length() > 0) {
      // We're opening an authenticated session
      props.setProperty("mail.smtp.auth", "true")
      props.setProperty("mail.smtp.starttls.enable", "true")
      props.setProperty("mail.smtp.starttls.required", "true")
    }
	    
    val session = Session.getInstance(props, null)
	
    session.setDebug(debug)
    
    session
  }
  
  def sendSystemEmail(recipient:Identity, subject:Wikitext, body:Wikitext):Try[Unit] = {
    val session = createSession()
    
    sendInternal(session, from, recipient.email, recipient.name, SystemUser.mainIdentity, subject, body)
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
    val session = createSession()
    
    val oidOptFuts = people map { person =>
      sendToPerson(context, person, session, subjectQL, bodyQL, from)
    }
    Future.sequence(oidOptFuts) map (_.flatten)
  }
  
  /**
   * The guts of actually sending email. Note that this can be invoked from a number of different circumstances,
   * some user-initiated and some not.
   */
  private def sendInternal(session:Session, from:String, 
      recipientEmail:EmailAddress, recipientName:String, requester:Identity, 
      subject:Wikitext, bodyMain:Wikitext):Try[Unit] = 
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
    
    val body = bodyMain + Wikitext("""
      |
      |------
      |
      |If you believe you have received this message in error, please drop a note to "betaemail@querki.net". (We're
      |working on the one-click unsubscribe, but not quite there yet; sorry.)""".stripMargin)
  	
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
  	    
    if (username.length > 0) {
      val transport = session.getTransport("smtp").asInstanceOf[SMTPTransport]
      transport.connect(username, password)
      transport.sendMessage(msg, msg.getAllRecipients)
      if (debug)
        QLog.spew(s"Sent email; transport returned ${transport.getLastServerResponse}")
    } else {
      // Non-TLS -- running on a test server, so just do it the easy way:
  	  Transport.send(msg)
    }
  }
	
  def sendToPerson(context:QLContext, person:Thing, session:Session, subjectQL:QLText, bodyQL:QLText, from:String)(implicit state:SpaceState):Future[Option[OID]] = {
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
        result = sendInternal(session, from, addr, name, context.request.requesterOrAnon.mainIdentity, subject, body)

      }
        yield result match {
          case Success(_) => Some(person.id)
          case Failure(ex) => Logger.error("Got an error while sending email ", ex); None 
        }
    }
  }
}
