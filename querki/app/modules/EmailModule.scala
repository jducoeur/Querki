package modules.email

import javax.mail._

import models._
import models.Space.oidMap
import models.Thing._
import models.system._

import ql.ContextBase

class EmailModule(val moduleId:Short) extends modules.Module {
  
  object MOIDs {
    val EmailTypeOID = moid(1)
    val EmailPropOID = moid(2)
  }  
  import MOIDs._
  
  override val types = Seq(EmailAddressType)
  
  override val props = Seq(EmailProp)
  
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
  object EmailAddressType extends EmailAddressType(EmailTypeOID)
  
  object EmailProp extends SystemProperty(EmailPropOID, EmailAddressType, Optional,
      toProps(
        setName("Email Address"),
        DisplayTextProp("""
This Property represents the general notion of something that can have an email
address. It is available on Person, but you can reuse it wherever you like.
          
Note, however, that this Property is one optional address. If you want to require
that an address be given, or allow a list of them, you will need to create a
separate Property with the Email Address type.
""")
      ))
}
