package querki

import models.Property
import querki.ecology._
import querki.globals._
import querki.values.RequestContext

package object history {
  import HistoryFunctions.HistoryVersion
  
  trait History extends EcologyInterface {
    def HistoryPerm:Property[OID,OID]
    
    def viewingHistoryVersion(rc:RequestContext):Option[HistoryVersion]
    def isViewingHistory(rc:RequestContext):Boolean
  }
}
