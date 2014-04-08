package querki

import models.{OID, Property, Thing}

import querki.ecology._
import querki.identity.User
import querki.values.SpaceState

package object security {
  
  object MOIDs extends EcotIds(4) {
    val CanEditCustomOID = moid(1)
    val PublicTagOID = moid(2)
    val MembersTagOID = moid(3)
    val OwnerTagOID = moid(4)
    val CanReadPropOID = moid(5)
    val CanEditPropOID = moid(6)
    val CanCreatePropOID = moid(7)
    val IsPermissionOID = moid(8)
    val CanEditChildrenPropOID = moid(9)
    val DefaultPermissionPropOID = moid(10)
    val PublicAllowedPropOID = moid(11)
  }

  trait AccessControl extends EcologyInterface {
    def PublicTag:Thing
    def MembersTag:Thing
    def OwnerTag:Thing
    
    // Checks whether this User *has* an Identity that is a Member of the Space. Use with caution!
    // Usage of this suggests a design bug!
    def isMember(who:User, state:SpaceState):Boolean
    // Checks whether an *Identity* is a Member of the Space. This is more conceptually correct.
    def isMember(identityId:OID, state:SpaceState):Boolean
    
    def definePermission(id:OID, name:String, summary:String, defaults:Seq[OID], publicAllowed:Boolean):Property[OID,OID]

    def canCreate(state:SpaceState, who:User, modelId:OID):Boolean
    def canRead(state:SpaceState, who:User, thingId:OID):Boolean
    def canEdit(state:SpaceState, who:User, thingIdIn:OID):Boolean
    def canChangePropertyValue(state:SpaceState, who:User, propId:OID):Boolean
    
    def CanCreateProp:Property[OID,OID]
    def CanEditProp:Property[OID,OID]
    def CanEditChildrenProp:Property[OID,OID]
    def CanReadProp:Property[OID,OID]
  }
  
  trait Encryption extends EcologyInterface {
    /**
     * Given a text String, this returns its hashed form, including the salt that was used
     * to generate the hash.
     */
    def calcHash(original:String):String
        
    /**
     * Does the provided original match the hash information?
     */
    def authenticate(original:String, rawHash:String):Boolean
  }
}