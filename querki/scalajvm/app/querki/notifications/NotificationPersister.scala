package querki.notifications

import akka.actor._

import querki.ecology._
import querki.identity.{UserId}

/**
 * The Actor that manages the DB side of persisting Notifications for a single User. Created
 * as part of this User's Session troupe.
 *
 * TODO: this currently has more than just Notifications. Should it get split?
 */
class NotificationPersister(
  val userId: UserId,
  implicit
  val ecology: Ecology
) extends Actor
     with EcologyMember {
  import NotificationPersister._

  lazy val NotificationPersistence = interface[NotificationPersistence]

  def receive = {
    // TODO: this doesn't really belong here:
    case LoadInfo => {
      NotificationPersistence.loadUserInfo(userId) match {
        case Some(userInfo) => sender ! userInfo
        case None           => throw new Exception("Couldn't load info for user " + userId + "!")
      }
    }

    case Load => {
      val current = NotificationPersistence.loadCurrent(userId)
      sender ! current
    }

    case UserNotificationActor.NewNotification(_, note) => {
      NotificationPersistence.createNotification(userId, note)
    }

    case UpdateLastChecked(lastChecked) => {
      NotificationPersistence.updateLastChecked(userId, lastChecked)
    }
  }
}

object NotificationPersister {
  case object Load

  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(
    ecology: Ecology,
    id: UserId
  ): Props = Props(new NotificationPersister(id, ecology))
}
