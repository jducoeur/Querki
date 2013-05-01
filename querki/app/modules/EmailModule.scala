package modules.email

import javax.activation.DataHandler
import javax.mail._
import javax.mail.internet._
import javax.mail.util.ByteArrayDataSource
import com.sun.mail.smtp._

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import modules.Modules._

import play.api.{Logger, Play}
import play.api.Play.current

class EmailModule(val moduleId:Short) extends modules.Module {

  def getRequiredConf(key:String) = {
    val fullKey = "querki.mail." + key
    val opt = Play.configuration.getString(fullKey)
    opt match {
      case Some(v) => v
      case None => throw new Exception("Didn't find required configuration key " + fullKey)
    }
  }
    
  lazy val smtpHost = getRequiredConf("smtpHost")
  lazy val from = getRequiredConf("from")

  object MOIDs {
    val EmailTypeOID = moid(1)
    val EmailPropOID = moid(2)
    val EmailTemplateOID = moid(3)
    val EmailToOID = moid(4)
    val EmailSendOID = moid(5)
    val EmailSubjectOID = moid(6)
    val EmailCcOID = moid(7)
    val EmailBodyOID = moid(8)
  }  
  import MOIDs._
  
  /******************************************
   * TYPES
   ******************************************/
  
  /**
   * Represents an email address. For the moment this is basically just a String, but we'll gradually
   * add things like validation, so it's useful to get the abstraction clean now.
   */
  case class EmailAddress(addr:String)
  class EmailAddressType(tid:OID) extends SystemType[EmailAddress](tid,
      toProps(
        setName("Email")
      )) with PTypeBuilder[EmailAddress,String]
  {
    def doDeserialize(v:String) = EmailAddress(v)
    def doSerialize(v:EmailAddress) = v.addr
    // TODO: in the long run, this probably should render as a clickable URL?
    def doRender(context:ContextBase)(v:EmailAddress) = Wikitext(v.addr)
    
    val doDefault = EmailAddress("")
    def wrap(raw:String):valType = EmailAddress(raw)
  }
  lazy val EmailAddressType = new EmailAddressType(EmailTypeOID)
  override lazy val types = Seq(EmailAddressType)
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  // The actual definition of this method is down below
  lazy val sendEmail = new SingleThingMethod(EmailSendOID, "Send Email", """Invoke this method to actually send this email.
      It will return a List of the Persons who the email was sent to this time.""", doSendEmail)
  
  lazy val emailAddress = new SystemProperty(EmailPropOID, EmailAddressType, Optional,
      toProps(
        setName("Email Address"),
        DisplayTextProp("""
This Property represents the general notion of something that can have an email
address. It is available on Person, but you can reuse it wherever you like.
          
Note, however, that this Property is one optional address. If you want to require
that an address be given, or allow a list of them, you will need to create a
separate Property with the Email Address type.
""")))
  
  lazy val emailTo = new SystemProperty(EmailToOID, LinkType, QList,
        toProps(
          setName("Email To"),
          (LinkModelOID -> Optional(ElemValue(Person.MOIDs.PersonOID))),
          DisplayTextProp("""
This is the raw list of people to send this email to. If you want to do
something fancier than sending to specific people, see the Recipients property.
""")))
  
  lazy val emailCc = new SystemProperty(EmailCcOID, LinkType, QList,
        toProps(
          setName("Email Cc"),
          (LinkModelOID -> Optional(ElemValue(Person.MOIDs.PersonOID))),
          DisplayTextProp("This is the raw list of people to copy on this email.")))
  
  lazy val emailSubject = new SystemProperty(EmailSubjectOID, TextType, ExactlyOne,
      toProps(
        setName("Email Subject"),
        DisplayTextProp("The title of the email")))

  lazy val emailBody = new SystemProperty(EmailBodyOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Email Body"),
        DisplayTextProp("The Contents of the email")))

  override lazy val props = Seq(
    // The actual email-address property
    emailAddress,
    
    // TODO: introduce the Recipients property. This is an indirection between
    // Email Message and Email To, a QL expression that returns the list of people
    // people to email.
    
    emailTo,
    
    emailCc,
    
    emailSubject,
    
    emailBody,
    
    sendEmail
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
        emailCc(),
        emailSubject(""),
        emailBody(),
        sendEmail(),
        DisplayTextProp("""
This is the Model for sending emails. You start by creating an Email Message. Then you
set its Display Text to say what you want (using all the same features you can use for
showing a Thing on the Web).
""")))
  )
  
  /***********************************************
   * METHOD CONTENTS
   ***********************************************/
    
  def doSendEmail(t:Thing, context:ContextBase) = {
    // TODO: this is where we actually send the email!!!
    Logger.info("Send Email has been called again on " + t.displayName)
    implicit val state = context.state
    val recipients = t.getProp(emailTo)
    
    // Construct the email:
    val props = System.getProperties()
    props.setProperty("mail.host", smtpHost)
    props.setProperty("mail.user", "querki")
    props.setProperty("mail.transport.protocol", "smtp")
    props.setProperty("mail.from", from)
    props.setProperty("mail.debug", "true")
    
    val session = Session.getInstance(props, null)

    // TODO: make this configurable:
    session.setDebug(true)
    
    def sendToPerson(person:Thing):Option[OID] = {
      try {
        // TODO: check if this person has already been sent this email
        val name = person.displayName
        val addrList = person.getProp(emailAddress)
        val addr = addrList.first
        // TBD: there must be a better way to do this. It's a nasty workaround for the fact
        // that setRecipients() is invariant, I believe.
        val toAddrs = Array(new InternetAddress(addr.addr, name).asInstanceOf[Address])
        
        val msg = new MimeMessage(session)
        msg.setFrom(new InternetAddress(from))

	    msg.setRecipients(Message.RecipientType.TO, toAddrs)
	    
	    val personContext = QLContext(TypedValue(ExactlyOne(ElemValue(person.id)), LinkType), context.request, Some(context))
	    
	    val subjectQL = t.getProp(emailSubject).first
	    val subjectParser = new QLParser(subjectQL, personContext)
	    val subject = subjectParser.process.plaintext
	    msg.setSubject(subject)
	    
	    val bodyQL = t.getProp(emailBody).first
	    val bodyParser = new QLParser(bodyQL, personContext)
	    val body = bodyParser.process.display
	    msg.setDataHandler(new DataHandler(new ByteArrayDataSource(body, "text/html")))
	    
	    msg.setHeader("X-Mailer", "Querki")
	    msg.setSentDate(new java.util.Date())
	    
	    Logger.info("About to send the message")
	    Transport.send(msg)

	    Some(person.id)
      } catch {
        case error:Throwable => Logger.info("Got an error while sending email " + error.getClass()); None 
      }
    }
    
    val sentTo = recipients.flatMap { personOID =>
      val thing = state.anything(personOID)
      thing match {
        case Some(person) => sendToPerson(person)
        case None => None  // TODO: some kind of error here?
      }
    }

    // TODO: add the recipients to the Sent To list
    
    val resultingList = QList.makePropValue(sentTo.map(ElemValue(_)).toList)
    TypedValue(resultingList, LinkType)
  }
}
