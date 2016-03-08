package querki

import models.{OID, Property, Thing}

import querki.ecology._
import querki.identity.User
import querki.values.SpaceState

package object security {

  trait AccessControl extends EcologyInterface {
    def PersonModel:Thing
    def PublicTag:Thing
    def MembersTag:Thing
    def OwnerTag:Thing
    def RoleModel:Thing
    
    // Checks whether this User *has* an Identity that is a Member of the Space. Use with caution!
    // Usage of this suggests a design bug!
    def isMember(who:User, state:SpaceState):Boolean
    // Checks whether an *Identity* is a Member of the Space. This is more conceptually correct.
    def isMember(identityId:OID, state:SpaceState):Boolean
    
    /**
     * Ecots should use this to define their own Permissions. Keep in mind that, as always, the OID *MUST* be
     * permanently stable, like any other OID in System.
     * 
     * If you are going to use this, your Ecot should depend upon AccessControl, and you should call this as
     * the definition of a lazy val, that gets referenced in your props collection, as usual for defining a
     * Property.
     */
    def definePermission(id:OID, name:String, summary:String, defaults:Seq[OID], publicAllowed:Boolean):Property[OID,OID]

    def canCreate(state:SpaceState, who:User, modelId:OID):Boolean
    def canRead(state:SpaceState, who:User, thingId:OID):Boolean
    def canEdit(state:SpaceState, who:User, thingIdIn:OID):Boolean
    def canChangePropertyValue(state:SpaceState, who:User, propId:OID):Boolean
    /**
     * The general-case permission checker. Ecots that define their own permissions should use this call to
     * test whether they are set on the current User.
     */
    def hasPermission(aclProp:Property[OID,_], state:SpaceState, who:User, thingId:OID):Boolean
    /**
     * This version checks whether a specific Identity has the given permission. You should usually favor this
     * call if possible, to preserve Identity Separation.
     */
    def hasPermission(aclProp:Property[OID,_], state:SpaceState, identityId:OID, thingId:OID):Boolean
    
    /**
     * Convenience function for fetching the Roles that this Person has.
     */
    def personRoles(person:Thing)(implicit state:SpaceState):Seq[Thing]
    
    def RolePermissionsProp:Property[OID,OID]
    def PersonRolesProp:Property[OID,OID]
    
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
  
  trait Roles extends EcologyInterface {
    /**
     * Fetches all of the Roles defined for this Space, in display order.
     */
    def allRoles(state:SpaceState):(Seq[Thing], Seq[Thing])
    
    def BasicMemberRole:Thing
    def CommentatorRole:Thing
    def ContributorRole:Thing
    def EditorRole:Thing
    def ManagerRole:Thing
    
    def CanExplorePerm:Property[OID,OID]
  }
}