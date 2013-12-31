package querki.access

import models._
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import querki.conventions.{PropDetails, PropSummary}

import querki.util._
import querki.values._

import modules.person.PersonModule._

import querki.identity.User

import play.api.Logger

/**
 * This is the public API for ACL management. The rest of Querki should work through here.
 */
object AccessControl {
  import modules.Modules.AccessControl._
  
  // TBD: this checks whether this person is a Member based on the Person records in the Space. Should we use
  // the SpaceMembership table instead? In general, there is a worrying semantic duplication here. We should
  // probably clarify the meaning of the Person record vs. the row in SpaceMembership.
  def isMember(who:User, state:SpaceState):Boolean = {
    implicit val s = state
    val members = state.descendants(modules.Modules.Person.person.id, false, true)
    members.exists { person =>
      val personIdentityOpt = person.getPropOpt(modules.Modules.Person.identityLink)
      personIdentityOpt.map { personIdentity =>
        val oid = personIdentity.first
        who.hasIdentity(oid)
      }.getOrElse(false)
    }
  }
  
  // TODO: this needs to become *much* more complete. But it's a start -- for the given permission,
  // we check whether it is defined on the Thing; if not, whether it is defined on the Space; and if
  // not, we use the provided default.
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, who:User, thingId:OID, default:Boolean, publicAllowed:Boolean):Boolean = {
    // TODO: this really ought to be who.isSuperadmin, instead of SystemUserOID?
    if (who.hasIdentity(state.owner) || who.id == SystemUserOID)
      true
    else {
      implicit val s = state
      val (isLocalUser, whoId) = who match {
        case _ => (isMember(who, state), who.id)        
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
        else if (perms.exists(who.hasPerson(_)))
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
  
  def canEdit(state:SpaceState, who:User, thingIdIn:OID):Boolean = {
    // Sadly, Edit turns out to be more complex than Create and Read -- simple inheritance of the value,
    // while conceptually elegant, doesn't actually work in practice. So we need to juggle two properties
    // instead.
//    hasPermission(canEditProp, state, who, thingIdIn, false, false)
    // TODO: refactor this with the hasPermission() method above.
    // TODO: this really ought to be who.isSuperadmin, instead of SystemUserOID?
    val thingId = { if (thingIdIn == UnknownOID) state.id else thingIdIn}
    if (who.hasIdentity(state.owner) || who.id == SystemUserOID)
      true
    else {
      implicit val s = state
      val (isLocalUser, whoId) = who match {
        case _ => (isMember(who, state), who.id)        
      }
      
      val thing = state.anything(thingId)
      
      // We check Who Can Edit on the Thing itself...
      val thingPermsOpt = thing.flatMap(_.localProp(canEditProp))
       
      def checkPerms(perms:PropAndVal[OID]):Boolean = {
        /* if (publicAllowed && perms.contains(MOIDs.PublicTagOID))
          true
        else */ if (isLocalUser && perms.contains(MOIDs.MembersTagOID))
          true
        else if (perms.exists(who.hasPerson(_)))
          true
        else
          // *NOT* default. If the properly exists, and is left unset, then we
          // always default to false!
          false          
      }
      
      // TODO: wow, that's a horror. Can we turn this into a well-behaved for comprehension or something?
      // Would Scalaz's "|" (or) operator help?
      thingPermsOpt.map(checkPerms(_)).getOrElse(
        thing.flatMap(_.getModelOpt.flatMap(_.getPropOpt(canEditChildrenProp)).map(checkPerms(_))).getOrElse(
          if (thingId == state.id)
            // Don't consider the Space to be its own child:
            false 
          else
            state.getPropOpt(canEditChildrenProp).map(checkPerms(_)).getOrElse(
              false)))
    }    
  }
  
  def canChangePropertyValue(state:SpaceState, who:User, propId:OID):Boolean = {
    implicit val s = state
    // TODO: for the time being, this is very simplistic: if the property is a permission,
    // then only the owner can change it. Otherwise, let it through. Later, we should
    // figure out how to expose field-level edit permissions to the UI.
    val hasPermissionOpt = for (
      prop <- state.prop(propId);
      permissionVal <- prop.getPropOpt(isPermissionProp);
      isPermission <- permissionVal.firstOpt;
      if isPermission
        )
      yield who.hasIdentity(state.owner)
      
    hasPermissionOpt.getOrElse(true)
  }
}

class AccessControlModule(val moduleId:Short) extends modules.Module {
  
  lazy val abstractPersonOID = modules.Modules.Person.MOIDs.SecurityPrincipalOID

  object MOIDs {
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
  
  lazy val isPermissionProp = new SystemProperty(IsPermissionOID, YesNoType, ExactlyOne,
      toProps(
        setName("_isPermission"),
        InternalProp(true),
        PropSummary("This Property is a Permission")))
  
  lazy val canEditCustomProp = new SystemProperty(CanEditCustomOID, QLType, Optional,
      toProps(
        setName("Who Can Edit Custom"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        PropSummary("Who else can edit this Thing")))

  lazy val canReadProp = new SystemProperty(CanReadPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Read"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        PropSummary("Who else can read Things in this Space")))

  lazy val canEditProp = new SystemProperty(CanEditPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        PropSummary("Who else can edit Things in this Space"),
        PropDetails("""Note that this Property is *not* inherited, unlike most. If you want to
            |say who can edit Things made from this Model, use [[Who Can Edit Children._self]] instead.""".stripMargin)))

  lazy val canEditChildrenProp = new SystemProperty(CanEditChildrenPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit Children"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        PropSummary("Who else can edit children of this Thing"),
        PropDetails("""This Property is useful on Models and Spaces, and works as follows.
            |
            |When you set this Property on a **Model**, it says who is allowed to edit the Things made from
            |that Model. That is, if I have a Model named *CD*, setting this Property on it says who can
            |edit the CDs.
            |
            |When you set this Property on a **Space**, it says who is generally allowed to edit Things in
            |the Space.
            |
            |Note that this differs from the ordinary [[Who Can Edit._self]] Property, which says who can
            |edit *this* specific Thing.""".stripMargin)))

  lazy val canCreateProp = new SystemProperty(CanCreatePropOID, LinkType, QSet,
      toProps(
        setName("Who Can Create"),
        SkillLevel(SkillLevel.Advanced),
        isPermissionProp(true),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        PropSummary("Who else can make new Things in this Space")))

  override lazy val props = Seq(
//    canEditCustomProp,
    isPermissionProp,
    canCreateProp,
    canEditProp,
    canEditChildrenProp,
    canReadProp
  )
}