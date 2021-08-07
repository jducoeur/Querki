package querki.identity

import akka.actor._

import querki.ecology._
import querki.util.QLog

private[identity] class UserCache(val ecology: Ecology) extends Actor with EcologyMember {

  import UserCacheMessages._

  lazy val UserAccess = interface[UserAccess]

  var usersByHandle = Map.empty[String, User]

  def receive = {
    case GetUserByHandle(handle) => {
      usersByHandle.get(handle) match {
        case Some(user) => sender ! UserFound(user)
        case None => {
          UserAccess.getUserByHandle(handle) match {
            case Some(user) => {
              usersByHandle += (handle -> user)
              sender ! UserFound(user)
            }
            case None => sender ! UserNotFound
          }
        }
      }
    }

    case UpdateUser(handle, user) => {
      usersByHandle += (handle -> user)
      sender ! UpdateAck
    }
  }
}

object UserCache {
  def actorProps(ecology: Ecology) = Props(classOf[UserCache], ecology)
}

object UserCacheMessages {

  sealed trait UserCacheRequest {
    def handle: String
  }

  case class GetUserByHandle(handle: String) extends UserCacheRequest

  sealed trait UserLookupResult
  case class UserFound(user: User) extends UserLookupResult
  case object UserNotFound extends UserLookupResult

  /**
   * This message tells the Cache to update the provided User record.
   */
  case class UpdateUser(
    handle: String,
    user: User
  ) extends UserCacheRequest
  case object UpdateAck
}
