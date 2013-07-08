package querki.access

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import querki.values._

import modules.Modules._

import identity.User

import play.api.Logger

/**
 * This is the public API for ACL management. The rest of Querki should work through here.
 */
object AccessControl {
  import modules.Modules.AccessControl._
  
  // TODO: this needs to become *much* more complete. But it's a start -- for the given permission,
  // we check whether it is defined on the Thing; if not, whether it is defined on the Space; and if
  // not, we use the provided default.
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, who:User, thingId:OID, default:Boolean, publicAllowed:Boolean):Boolean = {
    if (who.id == state.owner)
      true
    else {
      implicit val s = state
      val (isLocalUser, whoId) = who match {
        case modules.Modules.Person.SpaceSpecificUser(_, _, _, spaceId, localId) => ((spaceId == state.id), localId)
        case _ => (false, who.id)        
      }
      
      val thingPermsOpt =
        if (thingId == UnknownOID)
          None
        else {
          val thing = state.anything(thingId)
          thing.flatMap(_.getPropOpt(aclProp))
        }
      
      def checkPerms(perms:PropAndVal[OID]):Boolean = {
        if (publicAllowed && perms.contains(MOIDs.PublicTagOID))
          true
        else if (isLocalUser && perms.contains(MOIDs.MembersTagOID))
          true
        else if (perms.contains(whoId))
          true
        else
          // *NOT* default. If the properly exists, and is left unset, then we
          // always default to false!
          false          
      }
      
      thingPermsOpt.map(checkPerms(_)).getOrElse(
          state.getPropOpt(aclProp).map{checkPerms(_)}.getOrElse(default))
    }
  }
  
  def canCreate(state:SpaceState, who:User, modelId:OID):Boolean = {
    hasPermission(canCreateProp, state, who, modelId, false, false)
  }
  
  def canRead(state:SpaceState, who:User, thingId:OID):Boolean = {
    hasPermission(canReadProp, state, who, thingId, true, true)    
  }
  
  def canEdit(state:SpaceState, who:User, thingId:OID):Boolean = {
    hasPermission(canEditProp, state, who, thingId, false, false)
  }
}

class AccessControlModule(val moduleId:Short) extends modules.Module {
  
  lazy val abstractPersonOID = modules.Modules.Person.MOIDs.SecurityPrincipalOID

  object MOIDs {
    val CanEditCustomOID = moid(1)
    val PublicTagOID = moid(2)
    val MembersTagOID = moid(3)
    val OwnerTagOID = moid(4)
    val CanCreatePropOID = moid(4)
    val CanReadPropOID = moid(5)
    val CanEditPropOID = moid(6)
  }
  import MOIDs._
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val publicTag = ThingState(PublicTagOID, systemOID, abstractPersonOID,
      toProps(
        setName("Public"),
        DisplayTextProp("""
Use this Tag in Can Read if you want your Space or Thing to be readable by everybody.
""")))
    
  lazy val membersTag = ThingState(MembersTagOID, systemOID, abstractPersonOID,
      toProps(
        setName("Members"),
        DisplayTextProp("""
Use this Tag in Can Read if you want your Space or Thing to be readable by members of the Space.
""")))
    
  lazy val ownerTag = ThingState(OwnerTagOID, systemOID, abstractPersonOID,
      toProps(
        setName("Owner"),
        DisplayTextProp("""
Use this Tag in Can Read if you want your Space or Thing to be readable only by the owner and specific other people.
""")))
    
  override lazy val things = Seq(
    publicTag,
    membersTag,
    ownerTag
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val canEditCustomProp = new SystemProperty(CanEditCustomOID, QLType, Optional,
      toProps(
        setName("Who Can Edit Custom"),
        DisplayTextProp("This property says who else can edit this Thing")))

  lazy val canReadProp = new SystemProperty(CanReadPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Read"),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID))),
        DisplayTextProp("This property says who else can read Things in this Space")))

  lazy val canEditProp = new SystemProperty(CanEditPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit"),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID))),
        DisplayTextProp("This property says who else can edit Things in this Space")))

  lazy val canCreateProp = new SystemProperty(CanCreatePropOID, LinkType, QSet,
      toProps(
        setName("Who Can Create"),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID))),
        DisplayTextProp("This property says who else can make new Things in this Space")))

  override lazy val props = Seq(
//    canEditCustomProp,
    canCreateProp,
    canEditProp,
    canReadProp
  )
}