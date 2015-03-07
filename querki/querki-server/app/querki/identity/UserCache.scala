package querki.identity

import akka.actor._

import querki.ecology._
import querki.util.QLog

private [identity] class UserCache(val ecology:Ecology) extends Actor with EcologyMember {
  
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
    
    case GetAllUserIdsForAdmin(req) => req.requireAdmin {
      val userIds = UserAccess.getAllIdsForAdmin(req)
      sender ! AllUserIds(userIds)
    }
    
    case UpdateUser(user) => {
      user.identities.map { identity =>
        usersByHandle += (identity.handle -> user)
      }
      sender ! UpdateAck(user)
    }
  }
}

object UserCacheMessages {
  case class GetUserByHandle(handle:String)
  
  sealed trait UserLookupResult
  case class UserFound(user:User) extends UserLookupResult
  case object UserNotFound extends UserLookupResult
  
  /**
   * Note that only Admins are allowed to call this!
   * 
   * TODO: this is a scalability bug! We need to come up with more stream-compatible ways to do this!
   * This will fail horribly once we're past a few thousand users!
   */
  case class GetAllUserIdsForAdmin(req:User)  
  case class AllUserIds(users:Seq[UserId])
  
  /**
   * This message tells the Cache to update the provided User record.
   */
  case class UpdateUser(user:User)
  case class UpdateAck(user:User)
}
