package querki.admin

import scala.concurrent.Future

import querki.globals._

import querki.api.AdminFunctions
import AdminFunctions._
import querki.identity.UserLevel
import querki.session.{AutowireApiImpl, AutowireParams}

class AdminFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with AdminFunctions {
  
  // Head off illegal access here in the constructor, before we even try processing the request:
  if (!info.user.isAdmin)
    throw new Exception("Not allowed to use Admin functions!")
  
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  def statistics():Future[QuerkiStats] = {
    // TODO: this call is heavier-weight than we need. Slim it to just getting counts from the DB
    // TODO: this call really ought to return a Future, but we'll probably move to Slick streaming in
    // due course
    val allUsers = UserAccess.getAllForAdmin(info.user)
    
    val usersByLevel = allUsers.groupBy(_.level)
    def levelCount(level:UserLevel.UserLevel):Int = usersByLevel.get(level).map(_.size).getOrElse(0)
    
    val nPending = levelCount(UserLevel.PendingUser)
    val nFull = levelCount(UserLevel.FreeUser) + levelCount(UserLevel.PaidUser) + levelCount(UserLevel.PermanentUser)
    Future.successful(QuerkiStats(nPending, nFull, 0, 0))
  }
  
}
