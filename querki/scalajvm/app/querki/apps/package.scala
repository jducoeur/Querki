package querki

import models.Property

import querki.globals._
import querki.identity.User

/**
 * @author jducoeur
 */
package object apps {
  trait Apps extends EcologyInterface {
    def CanUseAsAppPerm:Property[OID,OID]
    def CanManipulateAppsPerm:Property[OID,OID]
    def ShadowFlag:Property[Boolean, Boolean]
    
    def addAppToSpace(user:User, spaceId:OID, appId:OID):Future[Unit]
  }
}