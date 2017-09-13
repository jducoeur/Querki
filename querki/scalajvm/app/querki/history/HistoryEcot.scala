package querki.history

import querki.ecology._
import querki.globals._
import querki.values.RequestContext

import HistoryFunctions._

object MOIDs extends EcotIds(65) {
  val FindAllStompedCmdOID = moid(1)
  val HistoryPermOID = moid(2)
}

class HistoryEcot(e:Ecology) extends QuerkiEcot(e) with History {
  import MOIDs._
  
  val cmds = new HistoryCommands(e)
  
  val AccessControl = initRequires[querki.security.AccessControl]
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[HistoryFunctions, HistoryFunctionsImpl](SpaceOps.spaceRegion, allowedDuringHistory = true)
  }
  
  def viewingHistoryVersion(rc:RequestContext):Option[HistoryVersion] = {
    rc.rawParam("_historyVersion") match {
      case Some(vStr) => {
        Some(vStr.toLong)
      }
      case _ => None
    }
  }
  
  def isViewingHistory(rc:RequestContext):Boolean = {
    rc.rawParam("_historyVersion").map(_.length > 0).getOrElse(false)
  }
  
  lazy val HistoryPerm = AccessControl.definePermission(
    HistoryPermOID,
    "Can View History", 
    "This permission controls whether someone is allowed to examine the History of this Space. This gives full read permission!", 
    Seq(AccessControl.OwnerTag),
    Seq(AccessControl.AppliesToSpace),
    false, 
    false)
    
  override lazy val props = Seq(
    HistoryPerm
  )
}
