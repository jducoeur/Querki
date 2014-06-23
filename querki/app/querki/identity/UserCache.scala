package querki.identity

import akka.actor._

import querki.ecology._

private[identity] class UserCache(val ecology:Ecology) extends Actor with EcologyMember {
  
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
  }
}

object UserCacheMessages {
  case class GetUserByHandle(handle:String)
  
  sealed trait UserLookupResult
  case class UserFound(user:User) extends UserLookupResult
  case object UserNotFound extends UserLookupResult
}