package querki.notifications

import akka.actor._

import anorm.{Success=>AnormSuccess,_}

import play.api.db._
import play.api.Play.current

// TEMP: while hacking the timestamps:
import com.github.nscala_time.time.Imports._

import models.OID

import querki.ecology._
import querki.identity.{Identity, UserId}
import querki.time.DateTime
import querki.values.QValue

/**
 * The Actor that manages the DB side of persisting Notifications for a single User. Created
 * as part of this User's Session troupe.
 * 
 * TODO: this currently has more than just Notifications. Should it get split?
 */
class NotificationPersister(val userId:UserId, implicit val ecology:Ecology) extends Actor with EcologyMember {
  import NotificationPersister._
  
  lazy val NotificationPersistence = interface[NotificationPersistence]

  def receive = {
    case LoadInfo => {
      NotificationPersistence.loadUserInfo(userId) match {
        case Some(userInfo) => sender ! userInfo
        case None => throw new Exception("Couldn't load info for user " + userId + "!")
      }
    }
    
    case Load => {
      // TODO: make this real:
      sender ! CurrentNotifications(Seq(
        Notification(3, Identity.AnonymousIdentity.id, Identity.AnonymousIdentity.id, NotifierId(1,1), DateTime.now - 15.days, None, None, Map.empty[OID,QValue]),
        Notification(2, Identity.AnonymousIdentity.id, Identity.AnonymousIdentity.id, NotifierId(1,1), DateTime.now - 25.days, None, None, Map.empty[OID,QValue]),
        Notification(1, Identity.AnonymousIdentity.id, Identity.AnonymousIdentity.id, NotifierId(1,1), DateTime.now - 90.days, None, None, Map.empty[OID,QValue])
      ))
    }
  }
}

object NotificationPersister {
  case object Load
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new NotificationPersister(id, ecology))
}