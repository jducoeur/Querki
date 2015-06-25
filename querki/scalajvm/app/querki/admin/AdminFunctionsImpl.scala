package querki.admin

import scala.concurrent.Future

import models.{AsOID, ThingId}

import querki.globals._

import querki.api._
import AdminFunctions._
import querki.data.TID
import querki.identity.{User, UserLevel}
import UserLevel._
import querki.session.{AutowireApiImpl, AutowireParams}
import querki.spaces.messages._

class AdminFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with AdminFunctions {
  
  // Head off illegal access here in the constructor, before we even try processing the request:
  if (!info.user.isAdmin)
    throw new NotAnAdminException
  
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  def doRoute(req:Request):Future[String] = route[AdminFunctions](this)(req)
  
  def statistics():Future[QuerkiStats] = {
    requestFuture[QuerkiStats] { implicit promise =>
      SpaceOps.spaceManager.request(GetSpaceCount(user)) foreach {
        case SpaceCount(nSpaces) => {
          // Okay -- we now know the number of Spaces, so let's get the user totals next:
        
	      // TODO: this call is heavier-weight than we need. Slim it to just getting counts from the DB
	      // TODO: this call really ought to return a Future, but we'll probably move to Slick streaming in
  	      // due course
	      val allUsers = UserAccess.getAllForAdmin(info.user)
	    
	      val usersByLevel = allUsers.groupBy(_.level)
	      val userCountsByLevel = usersByLevel.map { pair =>
	        val (level, users) = pair
	        (level -> users.size)
	      }
	      promise.success(QuerkiStats(userCountsByLevel, nSpaces))        
        }
      }      
    }
  }
  
  private def convertUser(user:User):AdminUserView = {
      val identity = user.mainIdentity
      AdminUserView(TID(user.id.toThingId.toString), identity.handle, identity.email.addr, user.level)    
  }
  private def convertUsers(users:Seq[User]):Future[Seq[AdminUserView]] = {
    val adminViews = users.map { convertUser(_) }
    Future.successful(adminViews)    
  }
  
  def pendingUsers():Future[Seq[AdminUserView]] = {
    convertUsers(UserAccess.getPendingForAdmin(info.user))
  }
  
  def allUsers():Future[Seq[AdminUserView]] = {
    convertUsers(UserAccess.getAllForAdmin(info.user))    
  }
  
  private def alterUserLevel(id:TID, level:UserLevel):Future[AdminUserView] = {
    ThingId(id.underlying) match {
      case AsOID(userId) => {
        val checkUser = UserAccess.getByUserId(user, userId).get
        if (checkUser.level == SuperadminUser)
          throw new Exception("No one is allowed to downgrade Superadmin!")
        if (checkUser.level == AdminUser && user.level != SuperadminUser)
          throw new Exception("Only Superadmin is allowed to downgrade an Admin!")
	    UserAccess.changeUserLevel(userId, user, level).map { 
	      case Some(newUser) => convertUser(newUser)
	      case None => throw new Exception(s"Failed to convert user $id!")
	    }
      }
      case _ => Future.failed(new Exception(s"Got non-OID UserId $id"))
    }    
  }
  
  def upgradePendingUser(id:TID):Future[AdminUserView] = {
    alterUserLevel(id, FreeUser)
  }
  
  def changeUserLevel(id:TID, level:UserLevel):Future[AdminUserView] = {
    level match {
      case SuperadminUser => throw new Exception("No one is allowed to create a Superadmin!")
      case AdminUser => if (user.level != SuperadminUser) throw new Exception("Only Superadmin is allowed to create Admins!")
      case _ => {}
    }
    
    alterUserLevel(id, level)
  }
}
