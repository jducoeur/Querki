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
    val EmailTypeOID = moid(1)
    val EmailPropOID = moid(2)
    val EmailTemplateOID = moid(3)
    val EmailToOID = moid(4)
    val EmailSendOID = moid(5)
    val EmailSubjectOID = moid(6)
//    val EmailCcOID = moid(7)
    val EmailBodyOID = moid(8)
    val EmailShowSendOID = moid(9)
    val SentToOID = moid(10)
    val RecipientsOID = moid(11)
  }  
  import MOIDs._
  
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
      It will return a List of the Persons who the email was sent to this time.""", sendEmailIfAllowed)
  
  lazy val showSendEmail = new SystemProperty(EmailShowSendOID, TextType, ExactlyOne,
      toProps(
        setName("Email Results")
      ))
  
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
  
  lazy val emailTo = new SystemProperty(EmailToOID, LinkType, QSet,
        toProps(
          setName("Email To"),
          (LinkModelOID -> Optional(ElemValue(Person.MOIDs.PersonOID))),
          DisplayTextProp("""
This is the raw list of people to send this email to. If you want to do
something fancier than sending to specific people, see the Recipients property.
""")))

  lazy val emailSubject = new SystemProperty(EmailSubjectOID, TextType, ExactlyOne,
      toProps(
        setName("Email Subject"),
        DisplayTextProp("The title of the email")))

  lazy val emailBody = new SystemProperty(EmailBodyOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Email Body"),
        DisplayTextProp("The Contents of the email")))
  
  lazy val sentToProp = new SystemProperty(SentToOID, LinkType, QSet,
      toProps(
        setName("Sent To"),
        DisplayTextProp("The Persons that this mail has already been sent to. (This is set automatically.)")))
  
  lazy val recipientsProp = new SystemProperty(RecipientsOID, QLType, ExactlyOne,
      toProps(
        setName("Recipients"),
        DisplayTextProp("""
The Recipients property declares who will receive this email. It is a QL expression, and
you should only modify it if you know what you are doing. By default, it simply defers to
the contents of the Email To property -- for ordinary email, you should just list the people
who are to receive this in Email To. But you can edit Recipients to be any other QL expression,
which can make some problems much easier. For example, if you want to send the email to
every Person listed in this Space, set Recipients to "Person._instances".
            
The QL expression given in here must product a List of Links to Persons.
        		""")))

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
{{form-horizontal:
{{control-group:
{{control-label:
**Subject**:
}}
{{controls:
[[Email Subject._edit]]
}}
}}
}}
{{form-horizontal:
{{control-group:
{{control-label:
**Recipients**: 
}}
{{controls:
[[Recipients._edit]]
}}
}}
}}
{{form-horizontal:
{{control-group:
{{control-label:
**Explicitly To**:
}}
{{controls:
[[Email To._edit]]
}}
}}
}}
{{form-horizontal:
{{control-group:
{{control-label:
**Already Sent To**:
}}
{{controls:
[[Sent To._edit]]
}}
}}
}}
{{form-horizontal:
{{control-group:
{{control-label:
**Body**:
}}
{{controls:
[[Email Body._edit]]
}}
}}
}}
    
------
            
[**Click here to send this email**](?prop=Email+Results)
""")))
  )
  
  /***********************************************
   * METHOD CONTENTS
   ***********************************************/
    
  import querki.access.AccessControl
  def sendEmailIfAllowed(t:Thing, context:ContextBase) = {
    if (AccessControl.canEdit(context.state, context.request.requesterOrAnon, t.id))
      doSendEmail(t, context)
    else
      TextValue("You aren't allowed to send that email")
  }
  
  def doSendEmail(t:Thing, context:ContextBase) = {
    implicit val state = context.state
    val recipientsIndirect = t.getProp(recipientsProp)
    val previouslySentToOpt = t.getPropOpt(sentToProp)
    
    // Get the actual list of recipients:
    val recipientParser = new QLParser(recipientsIndirect.first, t.thisAsContext(context.request))
    val recipientContext = recipientParser.processMethod
    if (recipientContext.value.pt != LinkType) {
      ErrorValue("The Recipient property of an Email Message must return a collection of Links; instead, it produced " + recipientContext.value.pt.displayName)
    } else {
      val recipients = recipientContext.value.v
      
	    // Construct the email:
	    val props = System.getProperties()
	    props.setProperty("mail.host", smtpHost)
	    props.setProperty("mail.user", "querki")
	    props.setProperty("mail.transport.protocol", "smtp")
	    props.setProperty("mail.from", from)
	    props.setProperty("mail.debug", "true")
	    
	    val session = Session.getInstance(props, null)
	
	    session.setDebug(debug)
	    
	    def sendToPerson(person:Thing):Option[OID] = {
	      try {
	        val name = person.displayName
	        val addrList = person.getProp(emailAddress)
	        if (addrList.isEmpty)
	          None
	        else {
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
		      
		      // Attach the HTML...
		      val bodyParser = new QLParser(bodyQL, personContext)
	          val bodyWikitext = bodyParser.process
		      val bodyHtml = bodyWikitext.display
		      val bodyPartHtml = new MimeBodyPart()
		      bodyPartHtml.setDataHandler(new DataHandler(new ByteArrayDataSource(bodyHtml, "text/html")))
		      
		      // ... and the plaintext...
		      val bodyPlain = bodyWikitext.plaintext
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
		    
		      Logger.info("About to send the message")
		      Transport.send(msg)
	
		      Some(person.id)
	        }
	      } catch {
	        case error:Throwable => Logger.info("Got an error while sending email " + error.getClass()); None 
	      }
	    }
	    
	    val sentTo = recipients.flatMap(LinkType) { personOID =>
	      if (previouslySentToOpt.isDefined && previouslySentToOpt.get.contains(personOID))
	        None
	      else {
	        val thing = state.anything(personOID)
	        thing match {
	          case Some(person) => sendToPerson(person)
	          case None => None  // TODO: some kind of error here?
	        }
	      }
	    }
	    val resultingList = QList.makePropValue(sentTo.map(ElemValue(_)).toList)
	    
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
	    
	    TypedValue(resultingList, LinkType)
    }
  }
}
