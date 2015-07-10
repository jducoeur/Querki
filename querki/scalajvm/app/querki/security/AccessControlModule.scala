package querki.security

import models._

import querki.util._
import querki.values._

import querki.ecology._
import querki.identity.{Identity, User}

import play.api.Logger
  
private object MOIDs extends EcotIds(4) {
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
  val HasPermissionFunctionOID = moid(12)
  val RoleModelOID = moid(13)
  val RolePermissionsOID = moid(14)
  val PersonRolesOID = moid(15)
}

class AccessControlModule(e:Ecology) extends QuerkiEcot(e) with AccessControl with querki.core.MethodDefs with querki.logic.YesNoUtils {
  
  import MOIDs._

  val Basic = initRequires[querki.basic.Basic]
  val Email = initRequires[querki.email.Email]
  val Links = initRequires[querki.links.Links]
  val Person = initRequires[querki.identity.Person]
  val Profiler = initRequires[querki.tools.Profiler]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
    
  lazy val QLType = Basic.QLType
  
  lazy val LinkModelProp = Links.LinkModelProp
  
  lazy val hasPermissionProfile = Profiler.createHandle("AccessControl.hasPermission")
  
  override def postInit() = {
    ApiRegistry.registerUserSessionImplFor[SecurityFunctions, SecurityFunctionsImpl](SpaceOps.spaceRegion)
  }
  
  // TBD: this checks whether this person is a Member based on the Person records in the Space. Should we use
  // the SpaceMembership table instead? In general, there is a worrying semantic duplication here. We should
  // probably clarify the meaning of the Person record vs. the row in SpaceMembership.
  // TODO: is this method simply broken conceptually? Shouldn't we be checking whether an *Identity* is a Member
  // of the Space? Is it ever appropriate for this to be the User that we're checking?
  def isMember(who:User, state:SpaceState):Boolean = {
    who.identities.exists(identity => isMember(identity.id, state))
  }

  // This code is intentionally duplicated from the above; I think this version is more correct, and should
  // probably supercede the other one.
  // TBD: this checks whether this person is a Member based on the Person records in the Space. Should we use
  // the SpaceMembership table instead? In general, there is a worrying semantic duplication here. We should
  // probably clarify the meaning of the Person record vs. the row in SpaceMembership.
  // TODO: this code is highly duplicative of stuff in PersonModule. It belongs in one or t'other, and should
  // be rationalized.
  def isMember(identityId:OID, state:SpaceState):Boolean = {
    Person.hasMember(identityId)(state)
  }
  
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, who:User, thingId:OID):Boolean = {
    who.identities.exists(identity => hasPermission(aclProp, state, identity.id, thingId))
  }
  
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, identityId:OID, thingId:OID):Boolean = {
    hasPermissionProfile.profile {
    
    if (identityId == state.owner || identityId == querki.identity.MOIDs.SystemIdentityOID)
      true
    else {
      implicit val s = state
      val isLocalUser = isMember(identityId, state)
      
      val thingPermsOpt =
        if (thingId == UnknownOID)
          None
        else {
          val thing = state.anything(thingId)
          thing.flatMap(_.getPropOpt(aclProp))
        }
      
      val publicAllowed = aclProp.firstOpt(PublicAllowedProp).getOrElse(false)
      
      def checkPerms(perms:PropAndVal[OID]):Boolean = {
        if (publicAllowed && perms.contains(PublicTagOID))
          true
        else if (isLocalUser && perms.contains(MembersTagOID))
          true
        else if (perms.exists(Person.isPerson(identityId, _)))
          true
        else
          // *NOT* default. If the properly exists, and is left unset, then we
          // always default to false!
          false          
      }
      
      def roleHasPerm(roleId:OID):Boolean = {
        val result = for {
          role <- state.anything(roleId)
          perms <- role.getPropOpt(RolePermissionsProp)
        }
          yield perms.contains(aclProp)
            
        result.getOrElse(false)
      }
        
      /**
       * Note that this never actually returns Some(false); it returns either Some(true) or None, to make it
       * easier to use.
       */
      def hasRole:Option[Boolean] = {  
        val result = for {
          person <- Person.localPerson(identityId)
          rolesPV <- person.getPropOpt(PersonRolesProp)
        }
          yield rolesPV.exists(roleHasPerm(_))
          
        result match {
          case Some(b) => if (b) Some(true) else None
          case _ => None
        }
      }
      
      // Try the permissions directly on this Thing...
      thingPermsOpt.map(checkPerms(_)).getOrElse(
          // ... or the Person has a Role that gives them the permission...
          // NOTE: this has to happen after Thing/Model, but before Space, since that is the semantic: Roles override
          // the Space settings, but are overridden by Thing/Model.
          hasRole.getOrElse(
          // ... or the permissions on the Space...
          state.getPropOpt(aclProp).map{checkPerms(_)}.getOrElse(
          // ... or the default permissions for this ACL...
          aclProp.getPropOpt(DefaultPermissionProp).map(checkPerms(_)).getOrElse(
          // ... or just give up and say no.
          false))))
    }
    
    }
  }
  
  def canCreate(state:SpaceState, who:User, modelId:OID):Boolean = {
    if (state.id == querki.ecology.SystemIds.systemOID)
      // You can't create anything in the System Space:
      false
    else
      hasPermission(CanCreateProp, state, who, modelId)
  }
  
  def canRead(state:SpaceState, who:User, thingId:OID):Boolean = {
    hasPermission(CanReadProp, state, who, thingId)    
  }
  
  def canEdit(state:SpaceState, who:User, thingIdIn:OID):Boolean = {
    hasPermissionProfile.profile {
    
    // Sadly, Edit turns out to be more complex than Create and Read -- simple inheritance of the value,
    // while conceptually elegant, doesn't actually work in practice. So we need to juggle two properties
    // instead.
    // TODO: refactor this with the hasPermission() method above.
    // TODO: this really ought to be who.isSuperadmin, instead of SystemUserOID?
    val thingId = { if (thingIdIn == UnknownOID) state.id else thingIdIn}
    val thing = state.anything(thingId)
    if (thing.isDefined && (thing.get.spaceId == querki.ecology.SystemIds.systemOID))
      // The System Space is not runtime-editable, regardless of who is asking:
      false
    else if (who.hasIdentity(state.owner) || who.id == querki.identity.MOIDs.SystemUserOID)
      true
    else {
      implicit val s = state
      val (isLocalUser, whoId) = who match {
        case _ => (isMember(who, state), who.id)        
      }
      
      
      // We check Who Can Edit on the Thing itself...
      val thingPermsOpt = thing.flatMap(_.localProp(CanEditProp))
       
      def checkPerms(perms:PropAndVal[OID]):Boolean = {
        /* if (publicAllowed && perms.contains(MOIDs.PublicTagOID))
          true
        else */ if (isLocalUser && perms.contains(MembersTagOID))
          true
        else if (perms.exists(Person.hasPerson(who, _)))
          true
        else
          // *NOT* default. If the properly exists, and is left unset, then we
          // always default to false!
          false          
      }
      
      def roleHasPerm(roleId:OID):Boolean = {
        val result = for {
          role <- state.anything(roleId)
          perms <- role.getPropOpt(RolePermissionsProp)
        }
          yield perms.contains(CanEditChildrenProp)
            
        result.getOrElse(false)
      }
        
      /**
       * Note that this never actually returns Some(false); it returns either Some(true) or None, to make it
       * easier to use.
       */
      def hasRole:Option[Boolean] = {  
        val result = for {
          identity <- Person.localIdentities(who).headOption
          person <- Person.localPerson(identity.id)
          rolesPV <- person.getPropOpt(PersonRolesProp)
        }
          yield rolesPV.exists(roleHasPerm(_))
          
        result match {
          case Some(b) => if (b) Some(true) else None
          case _ => None
        }
      }
      
      // TODO: wow, that's a horror. Can we turn this into a well-behaved for comprehension or something?
      // Would Scalaz's "|" (or) operator help?
      thingPermsOpt.map(checkPerms(_)).getOrElse(
        thing.flatMap(_.getModelOpt.flatMap(_.getPropOpt(CanEditChildrenProp)).map(checkPerms(_))).getOrElse(
          if (thingId == state.id)
            // Don't consider the Space to be its own child:
            false 
          else
            hasRole.getOrElse(
            state.getPropOpt(CanEditChildrenProp).map(checkPerms(_)).getOrElse(
              false))))
    }    
    
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
  
  def personRoles(person:Thing)(implicit state:SpaceState):Seq[Thing] = {
    val rolesOpt = for {
      rolesPV <- person.getPropOpt(PersonRolesProp)
      roleIds = rolesPV.rawList
      roles = roleIds.map(state.anything(_)).flatten
    }
      yield roles
      
    rolesOpt.getOrElse(Seq.empty[Thing])
  }
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  // SecurityPrincipal and PersonModel used to live in PersonModule. They were moved in order to avoid
  // order-of-initialization conflicts.
  
  lazy val SecurityPrincipal = ThingState(querki.identity.MOIDs.SecurityPrincipalOID, systemOID, querki.basic.MOIDs.SimpleThingOID,
      toProps(
        setName("Security Principal"),
        Summary("""For internal use -- this the concept of a Thing that can be given permissions.""")))
  
  lazy val PersonModel = ThingState(querki.identity.MOIDs.PersonOID, systemOID, querki.identity.MOIDs.SecurityPrincipalOID,
      toProps(
        setName("Person"),
        Core.InternalProp(true),
        Core.IsModelProp(true),
        // TODO: it is fundamentally suspicious that Email Address even exists on Person. It is convenient, and used in
        // PersonModule, but is (a) an information leak waiting to happen and (b) duplicate data. We really should always
        // be getting it from the Identity.
        //
        // The only real problem is that the Identity doesn't exist until the recipient *accepts* the invitation; that is
        // why it lives on the Person initially. But that's arguably a bug: we really should create an Identity for an
        // email address as soon as we first send an email to it, so that the owner of that email address can block all
        // further communications. (Unfortunately, but it needs to be an option.)
        Email.EmailAddressProp(Core.QNone),
        Summary("""This represents a Member of this Space.""")))
  
  lazy val PublicTag = ThingState(PublicTagOID, systemOID, SecurityPrincipal,
      toProps(
        setName("Public"),
        Summary("""
Use this Tag in Can Read if you want your Space or Thing to be readable by everybody.
""")))
    
  lazy val MembersTag = ThingState(MembersTagOID, systemOID, SecurityPrincipal,
      toProps(
        setName("Members"),
        Summary("""
Use this Tag in Can Read if you want your Space or Thing to be readable by members of the Space.
""")))
    
  lazy val OwnerTag = ThingState(OwnerTagOID, systemOID, SecurityPrincipal,
      toProps(
        setName("Owner"),
        Summary("""
Use this Tag in Can Read if you want your Space or Thing to be readable only by the owner and specific other people.
""")))

  lazy val RoleModel = ThingState(RoleModelOID, systemOID, SecurityPrincipal,
      toProps(
        setName("Role"),
        Core.IsModelProp(true),
        SkillLevel(SkillLevelAdvanced),
        // Concrete Roles should define their RolePermissions:
        RolePermissionsProp(),
        Summary("""Defines a Role that a Member of this Space can take, such as Contributor or Editor.
            |Each Role defines certain actions that the Member can take, such as commenting on Things or
            |contributing new ones.
            |
            |The built in Roles should suffice for most purposes, but if you need a new one, create a child
            |of this Model, add the desired permission Properties to it, and assign Members to the new Role.""".stripMargin)))
    
  override lazy val things = Seq(
    SecurityPrincipal,
    PersonModel,
    PublicTag,
    MembersTag,
    OwnerTag,
    RoleModel
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
        LinkModelProp(SecurityPrincipal),
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
        LinkModelProp(SecurityPrincipal),
        Summary("Who else can edit Things in this Space"),
        Details("""Note that this Property is *not* inherited, unlike most. If you want to
            |say who can edit Things made from this Model, use [[Who Can Edit Children._self]] instead.""".stripMargin)))

  lazy val CanEditChildrenProp = new SystemProperty(CanEditChildrenPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit Children"),
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(SecurityPrincipal),
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
  
  lazy val RolePermissionsProp = new SystemProperty(RolePermissionsOID, LinkType, QSet,
      toProps(
        setName("Role Permissions"),
        SkillLevel(SkillLevelAdvanced),
        // TODO: this really should set LinkModel to a Model that all Permissions are under, but we
        // don't have that concept yet:
        Links.LinkKindProp(Kind.Property),
        Summary("""This Property is only relevant to Roles. It defines the Permissions that are granted to all Members
            |of this Role.""".stripMargin)))
  
  lazy val PersonRolesProp = new SystemProperty(PersonRolesOID, LinkType, QSet,
      toProps(
        setName("Person Roles"),
        SkillLevel(SkillLevelAdvanced),
        Links.LinkModelProp(RoleModel),
        Summary("""This Property is only useful on Persons. It defines the Roles that this Person has.
            |You do not assign it directly; use the Sharing and Security page to manage which Roles each Person has.""".stripMargin)))
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val HasPermissionFunction = new InternalMethod(HasPermissionFunctionOID,
    toProps(
      setName("_hasPermission"),
      Summary("Produces true if the current user has the named permission on the received Thing"),
      Details("""    THING -> _hasPermission(PERMISSION._self) -> true or false
          |
          |Permission should be any Permission Property, such as Can Edit or Can Have User Values. It
          |is usually safe to assume that the current user Can Read, since they have already gotten to
          |this point.
          |
          |Note that you must include "._self" after the Permission's name, at least for the time being.
          |
          |This is typically used in _filter or _if.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      implicit val s = inv.state
      val resultInv = for {
        dummy <- inv.returnsType(YesNoType)
        thing <- inv.contextAllThings
        permId <- inv.processParamFirstAs(0, LinkType)
        propRaw <- inv.opt(inv.state.prop(permId))
        prop <- inv.opt(propRaw.confirmType(LinkType))
        who <- inv.opt(Person.localIdentities(inv.context.request.requesterOrAnon).headOption)
      }
        yield ExactlyOne(hasPermission(prop, inv.state, who.id, thing))
        
      if (resultInv.get.isEmpty)
        ExactlyOne(False)
      else
        resultInv        
    }
  }

  override lazy val props = Seq(
//    canEditCustomProp,
    isPermissionProp,
    CanCreateProp,
    CanEditProp,
    CanEditChildrenProp,
    CanReadProp,
    DefaultPermissionProp,
    RolePermissionsProp,
    PersonRolesProp,
    
    HasPermissionFunction
  )
}