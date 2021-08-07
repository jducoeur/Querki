package querki.notifications

import akka.actor.Props

import anorm.{Success => AnormSuccess, _}
import anorm.SqlParser._
import play.api.db._

import querki.db._
import ShardKind._
import querki.ecology._
import querki.identity.UserId
import querki.spaces.SerializedProps
import querki.time.TimeAnorm._
import querki.util.SqlHelpers._
import querki.values.SpaceState

object PersistenceMOIDs extends EcotIds(49)

class NotificationPersistenceEcot(e: Ecology) extends QuerkiEcot(e) with NotificationPersistence {

  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val SystemInterface = interface[querki.system.System]

  lazy val SystemState = SystemInterface.State

  // The name of the User's Notification Table
  def noteTable(id: UserId): String = "note" + id.toString

  def UserSQL(
    userId: UserId,
    query: String,
    version: Int = 0
  ): SqlQuery = {
    val replQuery = query.replace("{notename}", noteTable(userId))
    SQL(replQuery)
  }

  def loadUserInfo(userId: UserId): Option[UserNotificationInfo] = {
    QDB(System) { implicit conn =>
      SQL("""
          SELECT lastNoteChecked from User
           WHERE id = {id} 
          """)
        .on("id" -> userId.raw)
        .as(int("lastNoteChecked").map(UserNotificationInfo(userId, _)).singleOpt)
    }
  }

  def updateLastChecked(
    userId: UserId,
    lastChecked: Int
  ): Unit = {
    QDB(System) { implicit conn =>
      SQL("""
          UPDATE User
             SET lastNoteChecked = {lastChecked}
           WHERE id = {id}
          """).on("lastChecked" -> lastChecked, "id" -> userId.raw).executeUpdate
    }
  }

  /**
   * Parser for reading a single Notification row out of MySQL.
   */
  private val notificationParser =
    for {
      id <- int("id")
      sender <- oid("sender")
      to <- oid("toIdentity").?
      ecot <- int("ecotId")
      tpe <- int("noteType")
      sent <- dateTime("sentTime")
      spaceId <- oid("spaceId").?
      thingId <- oid("thingId").?
      props <- str("props")
      isRead <- bool("isRead")
      isDeleted <- bool("isDeleted")
    } yield Notification(
      id,
      sender,
      to,
      NotifierId(ecot.toShort, tpe.toShort),
      sent,
      spaceId,
      thingId,
      // Do we ever need a specific Space in order to deserialize a Notification? We probably can't,
      // since we are viewing them outside the context of the Space. So just use System.
      SerializedProps(props),
      isRead,
      isDeleted
    )

  def loadCurrent(userId: UserId): CurrentNotifications = {
    QDB(User) { implicit conn =>
      try {
        val notes = UserSQL(
          userId,
          """
            SELECT * from {notename}
             WHERE isDeleted = FALSE
             ORDER BY sentTime DESC
             LIMIT 100
            """
        )
          .as(notificationParser.*)
        CurrentNotifications(notes)
      } catch {
        case e: com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException => {
          // This is dealing with a very specific race condition that can occur. When a User is newly
          // created, the Client's request for numNewNotifications can race ahead of evolving this User's
          // state, with the result that we can hit this request before the user's Notifications table exists.
          // That results in this Exception. So we just quietly deal with it and return empty.
          CurrentNotifications(Seq.empty)
        }
        case e: Throwable => throw e
      }
    }
  }

  def createNotification(
    userId: UserId,
    note: Notification
  ) = {
    QDB(User) { implicit conn =>
      UserSQL(
        userId,
        """
          INSERT INTO {notename}
          ( id,   sender,   toIdentity,   ecotId,   noteType,   sentTime,   spaceId,   thingId,   props,   isRead,   isDeleted) VALUES
          ({id}, {sender}, {toIdentity}, {ecotId}, {noteType}, {sentTime}, {spaceId}, {thingId}, {props}, {isRead}, {isDeleted})
          """
      ).on(
        "id" -> note.id,
        "sender" -> note.sender.raw,
        "toIdentity" -> note.toIdentity.map(_.raw),
        "ecotId" -> note.notifier.ecotId,
        "noteType" -> note.notifier.notificationType,
        "sentTime" -> note.sentTime,
        "spaceId" -> note.spaceId.map(_.raw),
        "thingId" -> note.thingId.map(_.raw),
        "props" -> note.payload.ser,
        "isRead" -> note.isRead,
        "isDeleted" -> note.isDeleted
      ).executeUpdate
    }
  }

  def notificationPersisterProps(userId: UserId): Props = NotificationPersister.actorProps(ecology, userId)
}
