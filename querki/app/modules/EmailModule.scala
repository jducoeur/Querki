package modules.email

import javax.mail._

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql.{ContextBase, TypedValue}

import modules.Modules._

import play.api.Logger

class EmailModule(val moduleId:Short) extends modules.Module {
  
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
  
  lazy val sendEmail = new SingleThingMethod(EmailSendOID, "Send Email", """Invoke this method to actually send this email.
      It will return a List of the Persons who the email was sent to this time.""", 
    {
      (t:Thing, context:ContextBase) =>
        // TODO: this is where we actually send the email!!!
        Logger.info("Send Email has been called on " + t.displayName)
        implicit val state = context.state
        val recipients = t.getProp(emailTo)
        // TODO: this should only pass along the people who the email was actually sent to:
        TypedValue(recipients.v, LinkType)
    })
  
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
    new SystemProperty(EmailPropOID, EmailAddressType, Optional,
      toProps(
        setName("Email Address"),
        DisplayTextProp("""
This Property represents the general notion of something that can have an email
address. It is available on Person, but you can reuse it wherever you like.
          
Note, however, that this Property is one optional address. If you want to require
that an address be given, or allow a list of them, you will need to create a
separate Property with the Email Address type.
""")
      )),
      
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
}
