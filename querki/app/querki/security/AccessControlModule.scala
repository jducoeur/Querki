package querki.security

import models._
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import querki.util._
import querki.values._

import querki.ecology._
import querki.identity.User

import play.api.Logger

class AccessControlModule(e:Ecology) extends QuerkiEcot(e) with AccessControl {
  
  lazy val abstractPersonOID = querki.identity.MOIDs.SecurityPrincipalOID

  import MOIDs._
  
  lazy val Person = interface[querki.identity.Person]
  
  // TBD: this checks whether this person is a Member based on the Person records in the Space. Should we use
  // the SpaceMembership table instead? In general, there is a worrying semantic duplication here. We should
  // probably clarify the meaning of the Person record vs. the row in SpaceMembership.
  def isMember(who:User, state:SpaceState):Boolean = {
    implicit val s = state
    val members = state.descendants(Person.PersonModel.id, false, true)
    members.exists { person =>
      val personIdentityOpt = person.getPropOpt(Person.IdentityLink)
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
        else if (perms.exists(Person.hasPerson(who, _)))
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
    hasPermission(CanCreateProp, state, who, modelId, false, false)
  }
  
  def canRead(state:SpaceState, who:User, thingId:OID):Boolean = {
    hasPermission(CanReadProp, state, who, thingId, true, true)    
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
      val thingPermsOpt = thing.flatMap(_.localProp(CanEditProp))
       
      def checkPerms(perms:PropAndVal[OID]):Boolean = {
        /* if (publicAllowed && perms.contains(MOIDs.PublicTagOID))
          true
        else */ if (isLocalUser && perms.contains(MOIDs.MembersTagOID))
          true
        else if (perms.exists(Person.hasPerson(who, _)))
          true
        else
          // *NOT* default. If the properly exists, and is left unset, then we
          // always default to false!
          false          
      }
      
      // TODO: wow, that's a horror. Can we turn this into a well-behaved for comprehension or something?
      // Would Scalaz's "|" (or) operator help?
      thingPermsOpt.map(checkPerms(_)).getOrElse(
        thing.flatMap(_.getModelOpt.flatMap(_.getPropOpt(CanEditChildrenProp)).map(checkPerms(_))).getOrElse(
          if (thingId == state.id)
            // Don't consider the Space to be its own child:
            false 
          else
            state.getPropOpt(CanEditChildrenProp).map(checkPerms(_)).getOrElse(
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
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val publicTag = ThingState(PublicTagOID, systemOID, abstractPersonOID,
      toProps(
        setName("Public"),
        DisplayTextProp("""
Use this Tag in Can Read if you want your Space or Thing to be readable by everybody.
""")))
    
  lazy val MembersTag = ThingState(MembersTagOID, systemOID, abstractPersonOID,
      toProps(
        setName("Members"),
        DisplayTextProp("""
Use this Tag in Can Read if you want your Space or Thing to be readable by members of the Space.
""")))
    
  lazy val OwnerTag = ThingState(OwnerTagOID, systemOID, abstractPersonOID,
      toProps(
        setName("Owner"),
        DisplayTextProp("""
Use this Tag in Can Read if you want your Space or Thing to be readable only by the owner and specific other people.
""")))
    
  override lazy val things = Seq(
    publicTag,
    MembersTag,
    OwnerTag
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val isPermissionProp = new SystemProperty(IsPermissionOID, YesNoType, ExactlyOne,
      toProps(
        setName("_isPermission"),
        InternalProp(true),
        Summary("This Property is a Permission")))
  
  lazy val CanEditCustomProp = new SystemProperty(CanEditCustomOID, QLType, Optional,
      toProps(
        setName("Who Can Edit Custom"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        Summary("Who else can edit this Thing")))

  lazy val CanReadProp = new SystemProperty(CanReadPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Read"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        Summary("Who else can read Things in this Space")))

  lazy val CanEditProp = new SystemProperty(CanEditPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        Summary("Who else can edit Things in this Space"),
        Details("""Note that this Property is *not* inherited, unlike most. If you want to
            |say who can edit Things made from this Model, use [[Who Can Edit Children._self]] instead.""".stripMargin)))

  lazy val CanEditChildrenProp = new SystemProperty(CanEditChildrenPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit Children"),
        isPermissionProp(true),
        SkillLevel(SkillLevel.Advanced),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        Summary("Who else can edit children of this Thing"),
        Details("""This Property is useful on Models and Spaces, and works as follows.
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

  lazy val CanCreateProp = new SystemProperty(CanCreatePropOID, LinkType, QSet,
      toProps(
        setName("Who Can Create"),
        SkillLevel(SkillLevel.Advanced),
        isPermissionProp(true),
        (LinkModelOID -> Optional(ElemValue(abstractPersonOID, new DelegatingType(LinkType)))),
        Summary("Who else can make new Things in this Space")))

  override lazy val props = Seq(
//    canEditCustomProp,
    isPermissionProp,
    CanCreateProp,
    CanEditProp,
    CanEditChildrenProp,
    CanReadProp
  )
}