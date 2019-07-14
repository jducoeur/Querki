package querki

import models.{Thing, Property}
import querki.basic.PlainText
import querki.globals._
import querki.identity.User
import querki.values.RequestContext

/**
 * @author jducoeur
 */
package object apps {
  trait Apps extends EcologyInterface {
    val AppsTag = "Apps"
    
    def CanUseAsAppPerm:Property[OID,OID]
    def CanManipulateAppsPerm:Property[OID,OID]
    def ShadowFlag:Property[Boolean, Boolean]
    def GallerySummary:Property[PlainText, String]
    def GalleryDetails:Property[PlainText, String]
    def GalleryOwner:Property[OID,OID]
    def GalleryAppId:Property[OID,OID]
    def GalleryEntryId:Property[OID,OID]
    def IsAppFlag:Property[Boolean, Boolean]
    
    def addAppToSpace(rc: RequestContext, spaceId:OID, appId:OID):Future[Unit]
    def getShadowedThing(t:Thing)(implicit state:SpaceState):Thing
  }
}