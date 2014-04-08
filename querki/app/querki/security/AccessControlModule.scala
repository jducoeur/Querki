package querki.security

import models._

import querki.util._
import querki.values._

import querki.ecology._
import querki.identity.User

import play.api.Logger

class AccessControlModule(e:Ecology) extends QuerkiEcot(e) with AccessControl {
  
  import MOIDs._

  val Basic = initRequires[querki.basic.Basic]
  val Links = initRequires[querki.links.Links]
  val Person = initRequires[querki.identity.Person]
  
  lazy val QLType = Basic.QLType
  
  lazy val LinkModelProp = Links.LinkModelProp
  lazy val abstractPerson = Person.SecurityPrincipal
  
  // TBD: this checks whether this person is a Member based on the Person records in the Space. Should we use
  // the SpaceMembership table instead? In general, there is a worrying semantic duplication here. We should
  // probably clarify the meaning of the Person record vs. the row in SpaceMembership.
  // TODO: is this method simply broken conceptually? Shouldn't we be checking whether an *Identity* is a Member
  // of the Space? Is it ever appropriate for this to be the User that we're checking?
  def isMember(who:User, state:SpaceState):Boolean = {
    implicit val s = state
    val members = Person.members(state)
    members.exists { person =>
      val personIdentityOpt = person.getPropOpt(Person.IdentityLink)
      personIdentityOpt.map { personIdentity =>
        val oid = personIdentity.first
        who.hasIdentity(oid)
      }.getOrElse(false)
    }
  }

  // This code is intentionally duplicated from the above; I think this version is more correct, and should
  // probably supercede the other one.
  // TBD: this checks whether this person is a Member based on the Person records in the Space. Should we use
  // the SpaceMembership table instead? In general, there is a worrying semantic duplication here. We should
  // probably clarify the meaning of the Person record vs. the row in SpaceMembership.
  // TODO: this code is highly duplicative of stuff in PersonModule. It belongs in one or t'other, and should
  // be rationalized.
  def isMember(identityId:OID, state:SpaceState):Boolean = {
    implicit val s = state
    val members = Person.members(state)
    members.exists { person =>
      val personIdentityOpt = person.getPropOpt(Person.IdentityLink)
      personIdentityOpt.map { personIdentity =>
        val oid = personIdentity.first
        oid == identityId
      }.getOrElse(false)
    }
  }
  
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, who:User, thingId:OID):Boolean = {
    // TODO: this really ought to be who.isSuperadmin, instead of SystemUserOID?
    if (who.hasIdentity(state.owner) || who.id == querki.identity.MOIDs.SystemUserOID)
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
      
      val publicAllowed = aclProp.firstOpt(PublicAllowedProp).getOrElse(false)
      
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
      
      // Try the permissions directly on this Thing...
      thingPermsOpt.map(checkPerms(_)).getOrElse(
          // ... or the permissions on the Space...
          state.getPropOpt(aclProp).map{checkPerms(_)}.getOrElse(
          // ... or the default permissions for this ACL...
          aclProp.getPropOpt(DefaultPermissionProp).map(checkPerms(_)).getOrElse(
          // ... or just give up and say no.
          false)))
    }
  }
  
  def canCreate(state:SpaceState, who:User, modelId:OID):Boolean = {
    hasPermission(CanCreateProp, state, who, modelId)
  }
  
  def canRead(state:SpaceState, who:User, thingId:OID):Boolean = {
    hasPermission(CanReadProp, state, who, thingId)    
  }
  
  def canEdit(state:SpaceState, who:User, thingIdIn:OID):Boolean = {
    // Sadly, Edit turns out to be more complex than Create and Read -- simple inheritance of the value,
    // while conceptually elegant, doesn't actually work in practice. So we need to juggle two properties
    // instead.
    // TODO: refactor this with the hasPermission() method above.
    // TODO: this really ought to be who.isSuperadmin, instead of SystemUserOID?
    val thingId = { if (thingIdIn == UnknownOID) state.id else thingIdIn}
    if (who.hasIdentity(state.owner) || who.id == querki.identity.MOIDs.SystemUserOID)
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
  
  lazy val PublicTag = ThingState(PublicTagOID, systemOID, abstractPerson,
      toProps(
        setName("Public"),
        Summary("""
Use this Tag in Can Read if you want your Space or Thing to be readable by everybody.
""")))
    
  lazy val MembersTag = ThingState(MembersTagOID, systemOID, abstractPerson,
      toProps(
        setName("Members"),
        Summary("""
Use this Tag in Can Read if you want your Space or Thing to be readable by members of the Space.
""")))
    
  lazy val OwnerTag = ThingState(OwnerTagOID, systemOID, abstractPerson,
      toProps(
        setName("Owner"),
        Summary("""
Use this Tag in Can Read if you want your Space or Thing to be readable only by the owner and specific other people.
""")))
    
  override lazy val things = Seq(
    PublicTag,
    MembersTag,
    OwnerTag
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  def definePermission(id:OID, name:String, summary:String, defaults:Seq[OID], publicAllowed:Boolean = false):Property[OID,OID] = {
    new SystemProperty(id, LinkType, QSet,
      toProps(
        setName(name),
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(abstractPerson),
        Summary(summary),
        DefaultPermissionProp(defaults:_*),
        PublicAllowedProp(publicAllowed)))
  }
  
  lazy val isPermissionProp = new SystemProperty(IsPermissionOID, YesNoType, ExactlyOne,
      toProps(
        setName("_isPermission"),
        Core.InternalProp(true),
        Summary("This Property is a Permission")))
  
  lazy val CanEditCustomProp = new SystemProperty(CanEditCustomOID, QLType, Optional,
      toProps(
        setName("Who Can Edit Custom"),
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        Summary("Who else can edit this Thing")))

  lazy val CanReadProp = definePermission(CanReadPropOID, "Who Can Read", "Who else can read Things in this Space", Seq(PublicTag, OwnerTag), true)

  lazy val CanEditProp = new SystemProperty(CanEditPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit"),
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(abstractPerson),
        Summary("Who else can edit Things in this Space"),
        Details("""Note that this Property is *not* inherited, unlike most. If you want to
            |say who can edit Things made from this Model, use [[Who Can Edit Children._self]] instead.""".stripMargin)))

  lazy val CanEditChildrenProp = new SystemProperty(CanEditChildrenPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit Children"),
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(abstractPerson),
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

  lazy val CanCreateProp = definePermission(CanCreatePropOID, "Who Can Create", "Who else can make new Things in this Space", Seq(OwnerTag), false)
  
  lazy val DefaultPermissionProp = new SystemProperty(DefaultPermissionPropOID, LinkType, QSet,
      toProps(
        setName("_defaultPermission"),
        SkillLevel(SkillLevelAdvanced),
        // TODO: eventually, when we have user-defined permissions, we'll make this public:
        setInternal,
        // TODO: ideally, we'd like it to only apply to permissions:
        AppliesToKindProp(Kind.Property),
        Summary("Iff this Permission Property isn't set at all for a Thing, what values should be used?")))
  
  lazy val PublicAllowedProp = new SystemProperty(PublicAllowedPropOID, YesNoType, Optional,
      toProps(
        setName("_publicAllowed"),
        setInternal,
        AppliesToKindProp(Kind.Property),
        Summary("Set this on a Permission Property to allow Public as a legal value; otherwise, it will not be.")))

  override lazy val props = Seq(
//    canEditCustomProp,
    isPermissionProp,
    CanCreateProp,
    CanEditProp,
    CanEditChildrenProp,
    CanReadProp,
    DefaultPermissionProp
  )
}