package querki.security

import models._

import querki.api.commonName
import querki.ecology._
import querki.globals._
import querki.identity.{Identity, User}
import querki.spaces.{RTCAble, SpaceAPI, SpacePlugin, SpacePluginProvider}
import querki.values._
  
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
  val HasPermissionFunctionOID = moid(12)
  val RoleModelOID = moid(13)
  val RolePermissionsOID = moid(14)
  val PersonRolesOID = moid(15)
  val ChildPermissionsPropOID = moid(16)
  val InstancePermissionsPropOID = moid(17)
  val InstancePermissionsModelOID = moid(18)
  val IsInstancePermissionOID = moid(19)
  val CanDesignPermOID = moid(20)
  
  val PermAppliesToOID = moid(21)
  val AppliesToSpaceOID = moid(22)
  val AppliesToModelsOID = moid(23)
  val AppliesToInstancesOID = moid(24)
}

class AccessControlModule(e:Ecology) 
  extends QuerkiEcot(e) with AccessControl with querki.core.MethodDefs with querki.logic.YesNoUtils with SpacePluginProvider
{
  
  import MOIDs._

  val Basic = initRequires[querki.basic.Basic]
  val DeriveName = initRequires[querki.types.DeriveName]
  val Email = initRequires[querki.email.Email]
  val Links = initRequires[querki.links.Links]
  val Person = initRequires[querki.identity.Person]
  val Profiler = initRequires[querki.tools.Profiler]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Roles = interface[Roles]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
    
  lazy val QLType = Basic.QLType
  
  lazy val LinkModelProp = Links.LinkModelProp
  
  lazy val hasPermissionProfile = Profiler.createHandle("AccessControl.hasPermission")
  
  lazy val managerStateCacheKey = StateCacheKey(4, "managers")
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[SecurityFunctions, SecurityFunctionsImpl](SpaceOps.spaceRegion)
    SpaceChangeManager.registerPluginProvider(this)
  }
  
  /**
   * Called by each Space once, to instantiate its plugins. This is how we hook Space processing.
   */
  def createPlugin[RM[_]](space:SpaceAPI[RM], rtc:RTCAble[RM]):SpacePlugin[RM] = 
    new SecuritySpacePlugin(space, rtc, ecology)
  
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
  
  /**
   * Says whether this Identity has Manager rights within this Space. The Owner is always included, as is anybody
   * with the Manager Role.
   * 
   * This is deathly important: Managers can do *everything* that the Owner can do which is covered by permissions.
   * Only functions that are explicitly checked as "isOwner" are off-limits to Managers. This list should,
   * intentionally, be very short.
   * 
   * Since this needs to be checked frequently, we build a cache of Managers within the Space's dynamic cache. So
   * this only needs to be rebuilt when the Space changes.
   */
  def isManager(identityId: OID, state: SpaceState): Boolean = {
    implicit val s = state
    val managers = state.fetchOrCreateCache(managerStateCacheKey, {
      (Set[OID](state.owner) /: Person.members(state)) { (ids, member) =>
        val managerIdOpt = for {
          rolesPV <- member.getPropOpt(PersonRolesProp)
          if (rolesPV.rawList.contains(RolesMOIDs.ManagerOID))
          identity <- Person.getPersonIdentity(member)
        }
          yield identity.id
          
        ids ++ managerIdOpt
      }
    })
    managers.contains(identityId)
  }
  
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, who:User, thingId:OID):Boolean = {
    who.identities.exists(identity => hasPermission(aclProp, state, identity.id, thingId))
  }
  
  def hasPermission(aclProp:Property[OID,_], state:SpaceState, identityId:OID, thingId:OID):Boolean = {
    try {
      hasPermissionProfile.profile {
        if (isManager(identityId, state) || identityId == querki.identity.MOIDs.SystemIdentityOID)
          true
        else {
          implicit val s = state
          val thingOpt = state.anything(thingId)
          
          val isCreator = 
            (for {
              thing <- thingOpt
              creator <- thing.creatorOpt
              creatorIdentityId <- creator.identityIdOpt
            }
              yield (creatorIdentityId == identityId)
            ).getOrElse(false)
          
          lazy val isLocalUser = isMember(identityId, state)
          
          /**
           * Note that we check the Roles in two different ways: if a Role is explicitly named
           * in the ACL for this permission somewhere (fine-grained), or if this Role grants this
           * Permission space-wide (coarse-grained).
           */
          lazy val personRoles:Seq[OID] = {
            val raw = for {
              person <- Person.localPerson(identityId)
              rolesPV <- person.getPropOpt(PersonRolesProp)
            }
              yield rolesPV.rawList
              
            raw.getOrElse(Seq.empty)
          }
          
          lazy val publicAllowed = aclProp.firstOpt(PublicAllowedProp).getOrElse(false)
          
          /**
           * We've found the permission defined somewhere along the chain, so we're going to use this
           * setting. Does it let this person do this?
           */
          def checkPerms(perms:PropAndVal[OID]):Boolean = {
            if (publicAllowed && perms.contains(PublicTagOID))
              // Anybody's allowed, so just do it
              true
            else if (isLocalUser && perms.contains(MembersTagOID))
              // This person is allowed because they're a Member
              true
            else if (perms.exists(Person.isPerson(identityId, _)))
              // This person is explicitly named
              true
            else if (perms.exists(personRoles.contains(_)))
              // One of the Roles this person has is named
              true 
            else
              // *NOT* default. If the properly exists, and is left unset, then we
              // always default to false! The default is only used if the permission
              // isn't specified anywhere!
              false          
          }
  
          /**
           * This deals with checking the default Instance Permissions on the Space or the Model.
           */
          def checkInstancePermsFrom(t:Thing):Option[Boolean] = {
            for {
              instancePermsOID <- t.getFirstOpt(InstancePermissionsProp)
              instanceThing <- state.anything(instancePermsOID)
              perms <- instanceThing.getPropOpt(aclProp)
            }
              yield checkPerms(perms)
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
            if (personRoles.exists(roleHasPerm(_)))
              Some(true)
            else
              None
          }
          
          if (isCreator) {
            // At least for the time being, we allow the person who created a Thing to continue to do anything
            // to it.  We'll likely tighten that up over time, but for now it's a good starting point for allowing
            // Spaces to open up CanCreate without tying themselves in knots over CanEdit.
            true
          } else
            // Try the permissions directly on this Thing...
            thingOpt.flatMap(_.localProp(aclProp)).map(checkPerms(_)).getOrElse(
              // ... or try the Instance Permissions on its Model...
              thingOpt.flatMap(thing => thing.getModelOpt.flatMap(checkInstancePermsFrom(_))).getOrElse(
              // ... or the Person has a Role that gives them the permission...
              // NOTE: this has to happen after Thing/Model, but before Space, since that is the semantic: Roles override
              // the Space settings, but are overridden by Thing/Model.
              hasRole.getOrElse(
              // ... or the Instance Permissions on the Space, if that's not what we're looking at...
              {if (thingId == state.id) None else checkInstancePermsFrom(state)}.getOrElse(
              // ... or the default permissions for this ACL...
              aclProp.getPropOpt(DefaultPermissionProp).map(checkPerms(_)).getOrElse(
              // ... or just give up and say no.
              false)))))
        }
      }
    } catch {
      case ex:Exception => { QLog.error("Exception while checking permissions", ex); false }
    }
  }
  
  def canCreate(state:SpaceState, who:User, modelId:OID):Boolean = {
    if (state.id == querki.ecology.SystemIds.systemOID)
      // You can't create anything in the System Space:
      false
    else
      hasPermission(CanCreateProp, state, who, modelId)
  }
  
  def canDesign(state:SpaceState, who:User, modelId:OID):Boolean = {
    if (state.id == querki.ecology.SystemIds.systemOID)
      // You can't create anything in the System Space:
      false
    else
      hasPermission(CanDesignPerm, state, who, modelId)
  }
  
  def canRead(state:SpaceState, who:User, thingId:OID):Boolean = {
    hasPermission(CanReadProp, state, who, thingId)    
  }

  def canEdit(state:SpaceState, who:User, thingId:OID):Boolean = {
    if (state.id == querki.ecology.SystemIds.systemOID)
      // You can't edit anything in the System Space:
      false
    else {
      val perm = {
        // If this is a Model, use CanDesign; otherwise, use CanEdit.
        if (state.anything(thingId).map(_.isModel(state)).getOrElse(false))
          CanDesignPerm
        else
          CanEditProp
      }
      hasPermission(perm, state, who, thingId)
    }
  }

  def canChangePropertyValue(state:SpaceState, who:User, propId:OID):Boolean = {
    implicit val s = state
    // For the moment, the only thing we are filtering is Permissions, which require
    // the CanManageSecurity permission.
    val hasPermissionOpt = for {
      prop <- state.prop(propId)
      permissionVal <- prop.getPropOpt(isPermissionProp)
      isPermission <- permissionVal.firstOpt
      if isPermission
    }
      yield hasPermission(Roles.CanManageSecurityPerm, state, who, state.id) 
      
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
  
  def allPermissions(state:SpaceState):Iterable[Property[OID,_]] = {
    implicit val s = state
    state.
      allProps.
      map(_._2).
      filter(_.ifSet(isPermissionProp)).
      map(_.confirmType(LinkType).get)
  }
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  // SecurityPrincipal and PersonModel used to live in PersonModule. They were moved in order to avoid
  // order-of-initialization conflicts.
  
  lazy val SecurityPrincipal = ThingState(querki.identity.MOIDs.SecurityPrincipalOID, systemOID, RootOID,
    toProps(
      setName("Security Principal"),
      Categories(SecurityTag),
      Summary("""For internal use -- this the concept of a Thing that can be given permissions.""")))
  
  lazy val PersonModel = ThingState(querki.identity.MOIDs.PersonOID, systemOID, querki.identity.MOIDs.SecurityPrincipalOID,
    toProps(
      setName("Person"),
      Core.InternalProp(true),
      Core.IsModelProp(true),
      // This is to fix a specific edge case: when importing a Space, it might contain a Thing with the same
      // name as the Owner of the Space, causing all sorts of havoc. And more generally, when we add someone to
      // a Space, there might already be a Thing with that name, so we have to cope.
      DeriveName.DeriveNameProp(DeriveName.DeriveAlways),
      Categories(SecurityTag),
      Summary("""This represents a Member of this Space.""")))
  
  lazy val PublicTag = ThingState(PublicTagOID, systemOID, SecurityPrincipal,
    toProps(
      setName(commonName(_.security.public)),
      Categories(SecurityTag),
      Summary("""Use this Tag in Can Read if you want your Space or Thing to be readable by everybody.""")))
    
  lazy val MembersTag = ThingState(MembersTagOID, systemOID, SecurityPrincipal,
    toProps(
      setName(commonName(_.security.members)),
      Categories(SecurityTag),
      Summary("""Use this Tag in Can Read if you want your Space or Thing to be readable by members of the Space.""")))
    
  lazy val OwnerTag = ThingState(OwnerTagOID, systemOID, SecurityPrincipal,
    toProps(
      setName(commonName(_.security.owner)),
      Categories(SecurityTag),
      Summary("""Use this Tag in Can Read if you want your Space or Thing to be readable only by the owner and specific other people.""")))

  lazy val RoleModel = ThingState(RoleModelOID, systemOID, SecurityPrincipal,
    toProps(
      setName("Role"),
      Core.IsModelProp(true),
      SkillLevel(SkillLevelAdvanced),
      // Concrete Roles should define their RolePermissions:
      RolePermissionsProp(),
      Categories(SecurityTag),
      Summary("Defines a Role that a Member of this Space can take, such as Contributor or Editor."),
      Basic.DisplayTextProp("""Each Role defines certain actions that the Member can take, such as commenting on Things or
          |contributing new ones.
          |
          |The built in Roles should suffice for most purposes, but if you need a new one, create a child
          |of this Model, add the desired permission Properties to it, and assign Members to the new Role.""".stripMargin)))
          
  lazy val InstancePermissionsModel = ThingState(InstancePermissionsModelOID, systemOID, RootOID,
    toProps(
      setName("_Instance Permissions Model"),
      setInternal,
      // TODO (QI.7w4gasb): properly speaking, this should be anyone who has the Can Manage Security permission, but we don't
      // yet have a way for one permission to delegate to another:
      (CanEditPropOID -> QList(LinkType(RolesMOIDs.ManagerOID))),
      Summary("This is the Model for the Things that hold Default Permissions.")))
      
  lazy val AppliesToSpace = ThingState(AppliesToSpaceOID, systemOID, RootOID, 
    toProps(setName(commonName(_.security.appliesToSpace))))
  lazy val AppliesToModels = ThingState(AppliesToModelsOID, systemOID, RootOID, 
    toProps(setName(commonName(_.security.appliesToModels))))
  lazy val AppliesToInstances = ThingState(AppliesToInstancesOID, systemOID, RootOID, 
    toProps(setName(commonName(_.security.appliesToInstances))))
    
  override lazy val things = Seq(
    SecurityPrincipal,
    PersonModel,
    PublicTag,
    MembersTag,
    OwnerTag,
    RoleModel,
    InstancePermissionsModel,
    
    AppliesToSpace,
    AppliesToModels,
    AppliesToInstances
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  def definePermission(
    id:OID, 
    name:String, 
    summary:String, 
    defaults:Seq[OID],
    appliesTo:Seq[OID],
    isInstance:Boolean, 
    publicAllowed:Boolean = false):Property[OID,OID] = 
  {
    new SystemProperty(id, LinkType, QSet,
      toProps(
        setName(name),
        setInternal,
        // Permissions do not get edited in the traditional way:
        (querki.editing.MOIDs.NotEditableOID -> ExactlyOne(YesNoType(true))),
        // TODO (QI.7w4gasb): this isn't precisely correct. What it *should* do is allow anyone with the
        // Roles.CanManageSecurity Permission to do this. But we don't yet have a way for
        // a Permission to delegate to another Permission:
        (CanEditPropOID -> QList(LinkType(RolesMOIDs.ManagerOID))),
        isPermissionProp(true),
        IsInstancePermissionProp(isInstance),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(SecurityPrincipal),
        Categories(SecurityTag),
        Summary(summary),
        PermAppliesTo(appliesTo:_*),
        DefaultPermissionProp(defaults:_*),
        PublicAllowedProp(publicAllowed)))
  }
  
  lazy val isPermissionProp = new SystemProperty(IsPermissionOID, YesNoType, ExactlyOne,
      toProps(
        setName("_isPermission"),
        Core.InternalProp(true),
        Summary("This Property is a Permission")))
  
  lazy val IsInstancePermissionProp = new SystemProperty(IsInstancePermissionOID, YesNoType, ExactlyOne,
      toProps(
        setName("_isInstancePermission"),
        setInternal,
        Summary("This Property is an Instance Permission")))
  
  lazy val CanEditCustomProp = new SystemProperty(CanEditCustomOID, QLType, Optional,
      toProps(
        setName("Who Can Edit Custom"),
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        Summary("Who else can edit this Thing")))

  lazy val CanReadProp = 
    new SystemProperty(CanReadPropOID, LinkType, QSet,
      toProps(
        setName(commonName(_.security.canReadPerm)),
        setInternal,
        isPermissionProp(true),
        IsInstancePermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(SecurityPrincipal),
        Categories(SecurityTag),
        Summary("Who else can read Things in this Space"),
        DefaultPermissionProp(PublicTag),
        PublicAllowedProp(true),
        PermAppliesTo(AppliesToSpace, AppliesToModels, AppliesToInstances),
        // You specifically can *not* restrict visibility of Properties or Types, at least not yet:
        // that makes it way too easy to cause brokenness:
        Core.AppliesToKindProp(Kind.Thing, Kind.Space)))

  lazy val CanEditProp = new SystemProperty(CanEditPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit"),
        setInternal,
        isPermissionProp(true),
        IsInstancePermissionProp(true),
        PublicAllowedProp(false),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(SecurityPrincipal),
        ChildPermissionsProp(CanEditChildrenPropOID),
        PermAppliesTo(AppliesToSpace, AppliesToModels, AppliesToInstances),
        Categories(SecurityTag),
        Summary("Who else can edit Things in this Space"),
        Details("""Note that this Property is *not* inherited, unlike most. If you want to
            |say who can edit Things made from this Model, use [[Who Can Edit Children._self]] instead.""".stripMargin)))

  lazy val CanEditChildrenProp = new SystemProperty(CanEditChildrenPropOID, LinkType, QSet,
      toProps(
        setName("Who Can Edit Children"),
        setInternal,
        isPermissionProp(true),
        SkillLevel(SkillLevelAdvanced),
        LinkModelProp(SecurityPrincipal),
        Categories(SecurityTag),
        Basic.DeprecatedProp(true),
        Summary("Who else can edit children of this Thing"),
        Details("""**Deprecated:** this Permission no longer matters, and is no longer used for anything.
            |
            |This Property is useful on Models and Spaces, and works as follows.
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

  lazy val CanCreateProp = definePermission(CanCreatePropOID, 
      commonName(_.security.canCreatePerm), 
      "Who else can make new Instances in this Space", 
      Seq(OwnerTag),
      Seq(AppliesToSpace, AppliesToModels),
      true,
      false)
      
  lazy val CanDesignPerm = definePermission(CanDesignPermOID,
      commonName(_.security.canDesignPerm),
      "Who can design Models in this Space",
      Seq(OwnerTag),
      Seq(AppliesToSpace, AppliesToModels),
      true,
      false)
  
  lazy val DefaultPermissionProp = new SystemProperty(DefaultPermissionPropOID, LinkType, QSet,
      toProps(
        setName("_defaultPermission"),
        SkillLevel(SkillLevelAdvanced),
        // TODO: eventually, when we have user-defined permissions, we'll make this public:
        setInternal,
        // TODO: ideally, we'd like it to only apply to permissions:
        AppliesToKindProp(Kind.Property),
        Categories(SecurityTag),
        Summary("Iff this Permission Property isn't set at all for a Thing, what values should be used?")))
  
  lazy val PublicAllowedProp = new SystemProperty(PublicAllowedPropOID, YesNoType, Optional,
      toProps(
        setName("_publicAllowed"),
        setInternal,
        AppliesToKindProp(Kind.Property),
        Categories(SecurityTag),
        Summary("Set this on a Permission Property to allow Public as a legal value; otherwise, it will not be.")))
  
  lazy val RolePermissionsProp = new SystemProperty(RolePermissionsOID, LinkType, QSet,
      toProps(
        setName(commonName(_.security.rolePermissionsProp)),
        SkillLevel(SkillLevelAdvanced),
        // TODO: this really should set LinkModel to a Model that all Permissions are under, but we
        // don't have that concept yet:
        Links.LinkKindProp(Kind.Property),
        Categories(SecurityTag),
        Summary("""This Property is only relevant to Roles. It defines the Permissions that are granted to all Members
            |of this Role.""".stripMargin)))
  
  lazy val PersonRolesProp = new SystemProperty(PersonRolesOID, LinkType, QSet,
      toProps(
        setName(commonName(_.security.personRolesProp)),
        SkillLevel(SkillLevelAdvanced),
        Links.LinkModelProp(RoleModel),
        Categories(SecurityTag),
        Summary("""This Property is only useful on Persons. It defines the Roles that this Person has.
            |You do not assign it directly; use the Sharing and Security page to manage which Roles each Person has.""".stripMargin)))
  
  lazy val ChildPermissionsProp = new SystemProperty(ChildPermissionsPropOID, LinkType, Optional,
    toProps(
      setName("Child Permissions Property"),
      setInternal,
      Links.LinkKindProp(Kind.Property),
      Categories(SecurityTag),
      Summary("""Points from a Permission on *this* Thing to the one to check for its children."""),
      Details("""There are a small number of Permissions (as of this writing, only `Who Can Edit`), which are split
        |into one Property that you check on this specific Thing, and a related one that you check for its instances.
        |For example, `Who Can Edit`, on a Model, says who can edit the Model itself; `Who Can Edit Children` on that Model
        |says who can edit the "children" (usually the Instances) of that Model. In such cases, this meta-Property
        |points from, eg, `Who Can Edit` to `Who Can Edit Children`.
        |
        |This approach is a bit over-elaborate, and might yet evolve a bit. But for now, make sure to keep this
        |different in mind.""".stripMargin)))
  
  lazy val InstancePermissionsProp = new SystemProperty(InstancePermissionsPropOID, LinkType, ExactlyOne,
    toProps(
      setName("_Instance Permissions"),
      setInternal,
      Categories(SecurityTag),
      Summary("Points to the Thing that holds the Instance Permissions for this Space"),
      Details("""This Property points to a Thing where you place any default Permissions for all Instances.
        |
        |This is only temporarily visible -- it will shortly be covered by a UI that deals with this
        |stuff.""".stripMargin)))
  
  lazy val PermAppliesTo = new SystemProperty(PermAppliesToOID, LinkType, QSet,
    toProps(
      setName("_Permission Applies To"),
      setInternal,
      Categories(SecurityTag)))
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val HasPermissionFunction = new InternalMethod(HasPermissionFunctionOID,
    toProps(
      setName("_hasPermission"),
      Categories(SecurityTag),
      Summary("Produces true if the current user has the named permission on the received Thing"),
      Details("""```
          |THING -> _hasPermission(PERMISSION._self) -> true or false
          |```
          |
          |Permission should be any Permission Property, such as Can Edit or Can Have User Values. It
          |is usually safe to assume that the current user Can Read, since they have already gotten to
          |this point.
          |
          |Note that you must include "._self" after the Permission's name, at least for the time being.
          |
          |This is typically used in _filter or _if.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val s = inv.state
      val resultInv:QFut = for {
        dummy <- inv.returnsType(YesNoType)
        thing <- inv.contextAllThings
        permId <- inv.processParamFirstAs(0, LinkType)
        propRaw <- inv.opt(inv.state.prop(permId))
        prop <- inv.opt(propRaw.confirmType(LinkType))
        who <- inv.opt(Person.localIdentities(inv.context.request.requesterOrAnon).headOption)
      }
        yield ExactlyOne(hasPermission(prop, inv.state, who.id, thing))
        
      resultInv.map(result => if (result.isEmpty) ExactlyOne(False) else result)    
    }
  }

  override lazy val props = Seq(
//    canEditCustomProp,
    isPermissionProp,
    CanCreateProp,
    CanDesignPerm,
    CanEditProp,
    CanEditChildrenProp,
    CanReadProp,
    DefaultPermissionProp,
    RolePermissionsProp,
    PersonRolesProp,
    ChildPermissionsProp,
    InstancePermissionsProp,
    IsInstancePermissionProp,
    PermAppliesTo,
    
    HasPermissionFunction
  )
}
