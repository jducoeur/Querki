package querki.email

import models._
import ModelPersistence._

import querki.globals._
import querki.persistence._

object IdentityEmailPersistence {
  sealed trait IdentityEmailEvent
  
  case class DHEmailAddress(
    @KryoTag(1) addr:String
  ) extends UseKryo
    
  implicit def dh(email:EmailAddress):DHEmailAddress = DHEmailAddress(email.addr)
  
  case class DHSentEmail(
    @KryoTag(1) from:DHEmailAddress,
    @KryoTag(2) to:DHEmailAddress,
    @KryoTag(3) toName:String,
    @KryoTag(4) senderName:String,
    @KryoTag(5) subject:String,
    @KryoTag(6) body:String,
    @KryoTag(7) footer:String    
  ) extends UseKryo with IdentityEmailEvent
  
  implicit def dh(msg:EmailMsg):DHSentEmail = {
    DHSentEmail(
      msg.from,
      msg.to,
      msg.toName,
      msg.senderName,
      msg.subject.plaintext,
      msg.body.display,
      msg.footer.display
    )
  }
}
