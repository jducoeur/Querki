package querki.email

import akka.actor._
import Actor.Receive
import akka.persistence._

import querki.globals._
import querki.identity.{FullIdentity, IdentityId}
import querki.notifications._
import querki.persistence._
import querki.spaces.RTCAble

import IdentityEmailMessages._
import IdentityEmailPersistence._

/**
 * This is the core of the IdentityEmailActor, separated out as usual for testability.
 */
abstract class IdentityEmailCore[RM[_]](val rtc: RTCAble[RM])(implicit val ecology: Ecology)
  extends PersistentActorCore
     with PersistentRMCore[RM]
     with IdentityEmailPure
     with EcologyMember {
  lazy val Email = interface[querki.email.Email]
  lazy val Unsubscribe = interface[Unsubscribe]
  lazy val Notifications = interface[querki.notifications.Notifications]

  /////////////////////////////////////////////////////////
  //
  // External requirements
  //
  // These are the calls that currently need to be implemented in the actual Actor or test framework.
  //

  def identityId: OID

  def fetchIdentity(identityId: IdentityId): RM[Option[FullIdentity]]

  def toEmail(
    note: Notification,
    recipient: FullIdentity
  ): RM[EmailMsg]

  /////////////////////////////////////////////////////////

  def persistenceId: String = s"email$identityId"

  var _initializing = true

  var _identity: Option[FullIdentity] = None
  def identity = _identity.get

  var unsubsByNotifier: Map[NotifierId, List[UnsubEvent]] = Map.empty

  def addUnsub(
    notifierStr: String,
    evt: UnsubEvent
  ) = {
    val notifierId = NotifierId(notifierStr)
    val allEvts = unsubsByNotifier.get(notifierId) match {
      case Some(evts) => evt :: evts
      case _          => List(evt)
    }
    unsubsByNotifier += (notifierId -> allEvts)
  }

  def receiveRecover: Receive = {
    case UnsubscribeEvent(notifierStr, unsubEvt) => {
      addUnsub(notifierStr, unsubEvt)
    }

    case RecoveryCompleted => {
      fetchIdentity(identityId).map { identityOpt =>
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

  def receiveCommand: Receive = handleRequestResponse.orElse {
    // While we are initializing, just save everything away:
    case _ if (_initializing) => stash()

    case NotificationToEmail(note) => {
      Notifications.notifierFor(note).emailNotifier match {
        case Some(notifier) => {
          val unsubs = unsubsByNotifier.get(notifier.id)
          if (unsubs.isEmpty || notifier.shouldSendEmail(note, unsubs.get)) {
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
          } else {
            // Note that we drop the email on the floor if shouldSendEmail() returns false. That is *entirely*
            // intentional: for privacy reasons, the sender doesn't get feedback that they've been blocked.
//            QLog.spew(s"Dropping $note due to Unsubscription")
          }
        }
        case None => QLog.error(s"IdentityEmailCore somehow received notification without an EmailNotifier: $note")
      }
    }

    case DoUnsubscribe(to, notifierStr, unsubId, context) => {
      val notifier = Email.emailNotifier(NotifierId(notifierStr))
      val (message, unsubEvt) = notifier.getUnsubEvent(unsubId, context)
      val evt = UnsubscribeEvent(notifierStr, unsubEvt)
      doPersist(evt) { _ =>
        addUnsub(notifierStr, unsubEvt)
        respond(Unsubscribed(message))
      }
    }
  }
}
