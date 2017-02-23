package querki.email

import akka.actor._

import org.querki.requester._

import models.OID
import querki.globals._
import querki.identity.{FullIdentity, IdentityId}
import querki.notifications.Notification
import querki.persistence._
import querki.spaces.RealRTCAble
import querki.util.ClusterTimeoutChild

/**
 * This Sharded, Persistent Actor mediates all email being sent to a given Identity. Note
 * that an email address gains an Identity in the process of being sent its first email.
 */
class IdentityEmailActor(e:Ecology) 
  extends IdentityEmailCore[RequestM](RealRTCAble)(e) with Requester with PersistentQuerkiActor with ClusterTimeoutChild
{
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  def timeoutConfig = "querki.mail.identityTimeout"

  ///////////////////////////////////////////////////////////
  //
  // Implementations of IdentityEmailCore abstracts
  //
  
  def identityId:OID = OID(self.path.name)
  
  def fetchIdentity(identityId:IdentityId):RequestM[Option[FullIdentity]] = {
    IdentityAccess.getFullIdentity(identityId)
  }
  
  // This is ugly -- it is here solely to mediate Future -> RequestM. Can we do better?
  // As it stands, the hard Future here probably means that testing this is a little
  // problematic to begin with. Hmm.
  def toEmail(note:Notification, recipient:FullIdentity):RequestM[EmailMsg] =
    loopback(Notifications.notifierFor(note).emailNotifier.get.toEmail(note, recipient))
  
  ///////////////////////////////////////////////////////////
}

object IdentityEmailActor {
  def actorProps(ecology:Ecology):Props = Props(classOf[IdentityEmailActor], ecology)
}
