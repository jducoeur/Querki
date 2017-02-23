package querki.email

import akka.actor._
import Actor.Receive
import akka.persistence._

import querki.globals._
import querki.identity.{FullIdentity, IdentityId}
import querki.notifications.Notification
import querki.persistence._
import querki.spaces.RTCAble

import IdentityEmailMessages._
import IdentityEmailPersistence._

/**
 * This is the core of the IdentityEmailActor, separated out as usual for testability.
 */
abstract class IdentityEmailCore[RM[_]](val rtc:RTCAble[RM])(implicit val ecology:Ecology) 
  extends PersistentActorCore with PersistentRMCore[RM] with IdentityEmailPure with EcologyMember
{
  lazy val Email = interface[querki.email.Email]
  lazy val Notifications = interface[querki.notifications.Notifications]
  
  /////////////////////////////////////////////////////////
  //
  // External requirements
  //
  // These are the calls that currently need to be implemented in the actual Actor or test framework.
  //
  
  def identityId:OID
  
  def fetchIdentity(identityId:IdentityId):RM[Option[FullIdentity]]
  
  def toEmail(note:Notification, recipient:FullIdentity):RM[EmailMsg]
  
  /////////////////////////////////////////////////////////
  
  def persistenceId:String = s"email$identityId"
  
  var _initializing = true
  
  var _identity:Option[FullIdentity] = None
  def identity = _identity.get
  
  def receiveRecover:Receive = {
    case RecoveryCompleted => {
      fetchIdentity(identityId) map { identityOpt =>
        identityOpt match {
          case Some(identity) => {
            _identity = Some(identity)
            _initializing = false
            unstashAll()
          }
          case _ => {
            val ex = new Exception(s"IdentityEmailCore was unable to fetch the email address for Identity $identityId!")
            QLog.error("IdentityEmailCore recovery failed!", ex)
            throw ex
          }
        }
      }
    }
  }
  
  def receiveCommand:Receive = handleRequestResponse orElse {
    // While we are initializing, just save everything away:
    case _ if (_initializing) => stash()
    
    case NotificationToEmail(note) => {
      Notifications.notifierFor(note).emailNotifier match {
        case Some(notifier) => {
          // TODO: we need to pass Unsubs into here:
          if (notifier.shouldSendEmail(note)) {
            // Convert it to an email...
            toEmail(note, identity).map { msg =>
              // ... persist the email for introspection?
              // This was in the original plan, just for the sake of possible later introspection. But
              // in light of modern info-warfare, the emerging best practice is to *not* persist
              // information just for its own sake. So for now, we're *not* persisting the emails sent
              // on a per-recipient basis. If we come up with a reason to care about this, we might
              // actually turn this on later...
//              doPersist(dh(msg)) { _ =>
                // ... and send it out.
                Email.sendEmail(msg)
//              }
            }
          }
        }
        case None => QLog.error(s"IdentityEmailCore somehow received notification without an EmailNotifier: $note")
      }
    }
  }
}
