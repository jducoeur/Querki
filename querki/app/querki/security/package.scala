package querki

import models.{OID, Property, Thing}

import querki.ecology._
import querki.identity.User
import querki.values.SpaceState

import modules.ModuleIds

package object security {
  
  object MOIDs extends ModuleIds(4) {
    val CanEditCustomOID = moid(1)
    val PublicTagOID = moid(2)
    val MembersTagOID = moid(3)
    val OwnerTagOID = moid(4)
    val CanReadPropOID = moid(5)
    val CanEditPropOID = moid(6)
    val CanCreatePropOID = moid(7)
    val IsPermissionOID = moid(8)
    val CanEditChildrenPropOID = moid(9)
  }

  trait AccessControl extends EcologyInterface {
    def MembersTag:Thing
    def OwnerTag:Thing
    
    def isMember(who:User, state:SpaceState):Boolean
    
    def canCreate(state:SpaceState, who:User, modelId:OID):Boolean
    def canRead(state:SpaceState, who:User, thingId:OID):Boolean
    def canEdit(state:SpaceState, who:User, thingIdIn:OID):Boolean
    def canChangePropertyValue(state:SpaceState, who:User, propId:OID):Boolean
    
    def CanCreateProp:Property[OID,OID]
    def CanEditProp:Property[OID,OID]
    def CanEditChildrenProp:Property[OID,OID]
    def CanReadProp:Property[OID,OID]
  }
}