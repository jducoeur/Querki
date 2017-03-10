package querki.email

import models.Wikitext
import querki.identity.User
import querki.notifications.NotifierId
   
/**
 * Represents an email address. For the moment this is basically just a String, but we'll gradually
 * add things like validation, so it's useful to get the abstraction clean now.
 */
case class EmailAddress(addr:String)

case class UnsubInfo(notifier:NotifierId, user:User, email:EmailAddress, rest:List[String])

case class EmailMsg(
  from:EmailAddress,
  to:EmailAddress,
  toName:String,
  senderName:String,
  subject:Wikitext,
  body:Wikitext,
  footer:Wikitext
)
