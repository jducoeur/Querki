package querki.identity

object UserLevel {
  type UserLevel = Int
  
  val UnknownUserLevel = 0
  val PendingUser = 1
  val FreeUser = 2
  val PaidUser = 3
  val PermanentUser = 4
  
  val SpaceSpecific = 5
  
  val AdminUser = 10

  val TestUser = 20
  
  val SuperadminUser = 100
  
  def levelName(level:UserLevel) = level match {
    case PendingUser => "invited"
    case FreeUser => "free"
    case PaidUser => "paid"
    case PermanentUser => "permanent"
    case SpaceSpecific => "guest"
    case AdminUser => "admin"
  	case TestUser => "test"
    case SuperadminUser => "superadmin"
      
    case _ => "Unknown: " + level.toString
  }
  
  def isAdmin(level:UserLevel) = (level == AdminUser || level == SuperadminUser)
  
  def isActualUser(level:UserLevel) = level match {
    case PendingUser | FreeUser | PaidUser | PermanentUser | AdminUser | TestUser | SuperadminUser => true
    case _ => false
  }
}
