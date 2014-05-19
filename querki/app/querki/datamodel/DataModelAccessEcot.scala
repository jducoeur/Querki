package querki.datamodel

import querki.ecology._
import models.{Kind, PType}
import querki.ql.{QLCall, QLPhrase}
import querki.spaces.ThingChangeRequest
import querki.util.{Contributor, PublicException, Publisher}
import querki.values._
import querki.types.PropPath

object MOIDs extends EcotIds(21) {
  val InstancesMethodOID = sysId(44)
  val RefsMethodOID = sysId(48)
  val SpaceMethodOID = sysId(59)
  val ExternalRootsOID = sysId(60)
  val ChildrenMethodOID = sysId(62)
  val IsModelMethodOID = sysId(63)
  val PropsOfTypeOID = sysId(76)
  val IsDefinedOID = sysId(78)
  val AllPropsMethodOID = sysId(83)
  val OIDMethodOID = sysId(90)
  val KindMethodOID = sysId(91)
  val CurrentSpaceMethodOID = sysId(92)
  val IsMethodOID = sysId(93)
  val IsFunctionOID = sysId(104)
  
  val HasPropertyMethodOID = moid(1)
  val AllThingsOID = moid(2)
  val CopyIntoInstancesOID = moid(3)
  val AllTypesMethodOID = moid(4)
  val AsTypeMethodOID = moid(5)
  val ModelFunctionOID = moid(6)
}


/**
 * This Ecot mainly is about defining Functions that give QL access to the Ecology,
 * sliced and diced in various ways.
 * 
 * There is nothing sacred about the placement of most of these Functions. If a better
 * factoring is found, feel free to move them. (Pay attention to OIDs! But if it's a
 * sysId, it can be moved freely.)
 */
class DataModelAccessEcot(e:Ecology) extends QuerkiEcot(e) with DataModelAccess with querki.logic.YesNoUtils with querki.core.MethodDefs {
  import MOIDs._
  
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  override def init = {
    SpaceChangeManager.thingChanges += PropCopier
  }
  
  override def term = {
    SpaceChangeManager.thingChanges += PropCopier
  }
  
  lazy val QL = interface[querki.ql.QL]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Links = interface[querki.links.Links]
  lazy val PropPaths = interface[querki.types.PropPaths]
  lazy val Tags = interface[querki.tags.Tags]
  
  private object PropCopier extends Contributor[ThingChangeRequest, ThingChangeRequest] {
    // This is called whenever we get a Create or Modify request.
    def notify(evt:ThingChangeRequest, sender:Publisher[ThingChangeRequest,ThingChangeRequest]):ThingChangeRequest = {
      evt match {
        // We only worry about it if there is a Model and there isn't a Thing, so this is a Create:
        case ThingChangeRequest(state, Some(modelId), None, props, changedProps) => {
          implicit val s = state
          state.anything(modelId) match {
            case Some(model) => {
              val copiedProps = model.props.keys.filter { propId =>
                state.prop(propId) match {
                  case Some(prop) => prop.ifSet(CopyIntoInstances)
                  case None => false
                }
              }
              
              val newProps = (props /: copiedProps) { (curProps, copyId) =>
                if (props.contains(copyId))
                  // The create is already overriding the Model's value
                  curProps
                else
                  model.getPropOpt(copyId) match {
                    case Some(pv) => props + (copyId -> pv.v)
                    case None => props
                  }
              }
              
              ThingChangeRequest(state, Some(modelId), None, newProps, changedProps)
            }
            // TODO: this is actually a strange error -- what should we do here?
            case None => evt
          }
        }
        case _ => evt
      }
    }
  }
  
  def isDeletable(t:Thing)(implicit state:SpaceState):Boolean = {
    t match {
      case thing:ThingState => true
      case prop:Property[_,_] => {
        // You are allowed to delete a Property *if* nothing is using it any more:
        val thingsWithProp = state.allThings.filter(_.props.contains(prop.id))
        thingsWithProp.isEmpty
      }
      case _ => false
    }
  }
  
  lazy val DeletedValue:QValue = new QValue {
    override def isDeleted = true
    
    val cType = Core.QUnit
    def cv = ???
    def pType = ???
    
    override def toString = "DELETED QVALUE"
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/  
  
  lazy val CopyIntoInstances = new SystemProperty(CopyIntoInstancesOID, YesNoType, ExactlyOne, 
      toProps(
        setName("Copy into Instances"),
        SkillLevel(SkillLevelAdvanced),
        Core.AppliesToKindProp(Kind.Property),
        Summary("If set, this Property's values will always be copied from the Model into newly-created Instances"),
        Details("""Usually, Querki's Properties are inherited from the Model to the Instance -- that is, if the
            |Instance doesn't have the Property set, it will use the value from the Model. 99% of the time, that is
            |what you want -- it is flexible, and allows you to make changes to the Model and have them immediately
            |picked up by the Instances.
            |
            |Very occasionally, however, you have a Property that doesn't want this -- that wants to nail down the
            |value permanently when you create the Instance. For such Properties, set this flag. When the Instance
            |is created, the Model's value for this Property will be copied into the Instance, so that the Instance
            |permanently has its own value, and will not change if the Model does.
            |
            |This was mainly created for certain bulk-data-entry situations, where you want to set the value to something,
            |create a bunch of Instances with that value, then change it to something else and create a bunch of Instances
            |with that. This is reasonable, but keep in mind that creating sub-Models with the different values is
            |sometimes an easier and better way to deal with it.""".stripMargin)
        ))
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/  

  class InstancesMethod extends InternalMethod(InstancesMethodOID,
    toProps(
      setName("_instances"),
      Summary("Returns all of the non-Model Things that are based on this"),
      Details("""A Model is sort of like the concept of a Thing: "Person" or "CD" or "Recipe".
          |
          |An Instance is an actual Thing based on one of those Models: "Joe" or "In Through the Out Door" or "Macaroni and Cheese".
          |
          |Most of the time, when using Querki, you want to create one or more Models that describe the *kinds*
          |of Things you're interested in, and then create a bunch of Instances of each Model.
          |
          |So _instances looks like this:
          |    MODEL -> _instances -> LIST OF INSTANCES
          |That is, it receives a *Model*, and produces the Instances that come from that Model.
          |
          |If you have sub-Models under *Model* (that add more Properties, for example), this will include those as well.""".stripMargin)))
  {
    override def qlApply(invIn:Invocation):QValue = {
      val inv = invIn.preferDefiningContext
      for (
        thing <- inv.contextAllThings
      )
        yield Core.listFrom(inv.state.descendants(thing.id, false, true).map(_.id), LinkType)
    }
  }
  
  class RefsMethod extends InternalMethod(RefsMethodOID, 
    toProps(
      setName("_refs"),
      Summary("""Returns all of the Things that use this Property to point to this Thing."""),
      Details("""    THING -> PROPERTY._refs -> REFERRING THINGS
          |Say that my Space is listing my CD collection. I have a Model *Album* for individual discs,
          |and Model *Artist* for performers. Album has a Property *Artists*, which is a Set of Links
          |to Artist -- basically, the list of performers on this particular CD.
          |
          |In this case, *Artist* is likely to want to say something like:
          |[[_code(""[[Artists._refs -> _bulleted]]"")]]
          |That is, based on the Artist we're looking at (which is always the initial Context passed into
          |a QL Expression), get all the Things that refer to this Artist using the *Artists* Property,
          |and display them as a bullet list.
          |
          |This method is enormously useful -- most Models that get pointed to like this will probably
          |want to use it.
          |
          |Note that this always returns a List, since any number of Things could be pointing to this.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        dummy <- inv.preferCollection(QList)
        prop <- inv.definingContextAsPropertyOf(LinkType)
        // Build the list of possible paths once, since it's a fairly intensive process:
        paths = PropPaths.pathsToProperty(prop)(inv.state)
        thing <- inv.contextAllThings
        candidateThing <- inv.iter(inv.state.allThings)
        path <- inv.iter(paths)
        propAndVal <- inv.iter(path.getPropOpt(candidateThing)(inv.state))
        if (propAndVal.contains(thing.id))
      }
        yield ExactlyOne(LinkType(candidateThing.id))
    }
  }
  
  class SpaceMethod extends SingleThingMethod(SpaceMethodOID, "_space", "What Space is this Thing in?", 
    """    RECEIVED -> _space -> SPACE
    |
    |This function produces the Space that the received Thing is contained in.""".stripMargin,
  { (thing, context) => Links.LinkValue(thing.spaceId) })

  class ExternalRootsMethod extends SingleThingMethod(ExternalRootsOID, "_externalRoots", "What are the ancestor Things for this Space?", 
    """    SPACE -> _externalRoots -> ROOTS
    |
    |Pass in a link to a Space; this produces all of the "roots" -- the Things from its Apps -- used
    |by that Space.
    |
    |User code will rarely care about this function, but it is part of how the [[All Things._self]] display works.""".stripMargin,
  { (thing, context) => 
    implicit val state = context.state
    val thingRoots:Iterable[OID] = {
      ((Set.empty[OID] /: state.allThings) ((set, t) => set + state.root(t))).
        filterNot(oid => state.anything(oid).map(_.ifSet(Core.InternalProp)).getOrElse(false))
    }
    
    Core.listFrom(thingRoots, LinkType) 
  })

  class AllPropsMethod extends SingleThingMethod(AllPropsMethodOID, "_allProps", "What are the Properties in this Space?", 
    """    SPACE -> _allProps -> PROPS
    |
    |This receives a link to a Space, and produces all of the Properties defined in that Space.""".stripMargin,
  { (thing, context) => 
    thing match {
      case s:SpaceState => Core.listFrom(s.propList.toSeq.sortBy(_.displayName), Core.LinkFromThingBuilder) 
      case _ => QL.WarningValue("_allProps must be used with a Space")
    }
  
  })

  lazy val AllTypesMethod = new SingleThingMethod(AllTypesMethodOID, "_allTypes", "What are the Types in this Space?", 
    """    SPACE -> _allTypes -> TYPES
    |
    |This receives a link to a Space, and produces all of the Types defined in that Space.""".stripMargin,
  { (thing, context) => 
    thing match {
      case s:SpaceState => Core.listFrom(s.types.values.toSeq.sortBy(_.displayName), Core.LinkFromThingBuilder) 
      case _ => QL.WarningValue("_allTypes must receive a Space")
    }
  
  })
  
  class ChildrenMethod extends SingleThingMethod(ChildrenMethodOID, "_children", "This produces the immediate children of the received Model.",
    """    MODEL -> _children -> LIST OF CHILDREN
    |This produces all of the Things that list MODEL as their Model. It includes both other Models, and Instances.""".stripMargin,
  { (thing, context) => Core.listFrom(context.state.children(thing).map(_.id), LinkType) })

  class IsModelMethod extends SingleThingMethod(IsModelMethodOID, "_isModel", "This produces Yes if the received Thing is a Model.",
    """    THING -> _isModel -> Yes or No""".stripMargin,
  { (thing, context) => ExactlyOne(thing.isModel(context.state)) })

  class IsDefinedMethod extends InternalMethod(IsDefinedOID,
    toProps(
      setName("_isDefined"),
      Summary("Produces Yes if the name passed into it is a real Thing"),
      Details("""    NAME -> _isDefined -> YES or NO
          |You typically use _isDefined with a Tag Property. It is simply a way to ask "is there actually something
          |with this name?", so that you can handle it differently depending on whether there is or not.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        dummy <- inv.returnsType(YesNoType)
        elem <- inv.contextElements
        qv <- inv.contextValue
      }
        yield ExactlyOne(qvIsReal(qv)(inv.state))
    }
    
    def qvIsReal(qv:QValue)(implicit state:SpaceState):Boolean = {
      if (qv.pType == QL.UnknownNameType)
        false
      else if (qv.pType == Tags.NewTagSetType)
        state.anythingByName(qv.firstAs(Tags.NewTagSetType).get.text).isDefined
      else if (qv.pType == Tags.TagSetType)
        state.anythingByName(qv.firstAs(Tags.TagSetType).get).isDefined
      else if (qv.pType == LinkType)
        state.anything(qv.firstAs(LinkType).get).isDefined
      else
        false
    }
  }

  class OIDMethod extends SingleThingMethod(OIDMethodOID, "_oid", "Get the unique global id of this Thing", 
    """    THING -> _oid -> Text
    |
    |This function produces the unique Object ID (which will generally be a period followed by some letters and numbers)
    |of the received Thing.
    |
    |Each Thing in Querki has an Object ID. In most cases, it can be used in place of the Thing's name, and it is never
    |ambiguous -- it always refers to one specific Thing.""".stripMargin,
  { (thing, context) => Basic.TextValue(thing.id.toThingId) })

  class KindMethod extends InternalMethod(KindMethodOID,
    toProps(
      setName("_kind"), 
      Summary("What kind of Thing is this?"), 
      Details("""There are two ways to use _kind:
          |    THING -> _kind -> Number
          |
          |This function produces the Number that represents the "kind"
          |of the received Thing. The Kinds are:
          |
          |* Thing: 0
          |* Type: 1
          |* Property: 2
          |* Space: 3
          |* Collection: 4
          |* Attachment: 5
          |
          |By and large, though, you should never use these numbers directly. Instead, use
          |the second form of this method:
          |    _kind(KIND) -> Number
          |The KIND parameter should be exactly one of the above names.
          |
          |So for example, you can test whether the incoming Thing is a Property by saying:
          |    ... -> _if(_equals(_kind, _kind(Property)), ...)
          |""".stripMargin)))
  { 
  override def qlApply(inv:Invocation):QValue = {
    val paramsOpt = inv.paramsOpt
    
    // If there is a parameter, this will produce its value:
    val paramResult = for (
      params <- paramsOpt;
      param = params(0);
      QLCall(kindName, _, _, _) = param.ops(0)
        )
      yield Kind.fromName(kindName.name).map(kind => ExactlyOne(IntType(kind))).getOrElse(WarningValue("Unknown Kind: " + kindName))
      
    // If not, produce the incoming Thing's value:
    paramResult.getOrElse(applyToIncomingThing(inv) { (thing, context) => ExactlyOne(IntType(thing.kind)) })
  }
  }

  class CurrentSpaceMethod extends SingleThingMethod(CurrentSpaceMethodOID, "_currentSpace", "What Space are we looking at?", 
    """THING -> _currentSpace -> SPACE
    |
    |This function produces the Space that we are currently displaying. (Generally, the one in the URL.)""".stripMargin,
  { (thing, context) => Links.LinkValue(context.root.state) })

  class IsMethod extends InternalMethod(IsMethodOID,
    toProps(
      setName("_is"),
      Summary("Allows you to test whether you have a specific Thing"),
      Details("""    THING -> _is(THING) -> Yes or No
    |
    |This function produces Yes iff the parameter matches the passed-in THING, and No otherwise. It is almost always used
    |inside _if(). For instance, to check whether a Property is of Text Type:
    |    MyProp.Property Type -> _if(_is(Text Type), ...)""".stripMargin)))
  { 
    override def qlApply(inv:Invocation):QValue = {
      for 
      (
        receivedId <- inv.contextAllAs(LinkType);
        paramId <- inv.processParamFirstAs(0, LinkType)
      )
        yield ExactlyOne(receivedId == paramId)
    }
  }

	lazy val PropsOfTypeMethod = new SingleThingMethod(PropsOfTypeOID, "_propsOfType", "This receives a Type, and produces all of the Properties in this Space with that Type",
	    """    TYPE -> _propsOfType -> LIST OF PROPS""".stripMargin,
	{ (thing, context) =>
	  thing match {
	    case pt:PType[_] => Core.listFrom(context.state.propsOfType(pt), Core.LinkFromThingBuilder)
	    case _ => QL.WarningValue("_propsOfType can only be used on a Type")
	  }
	})

  lazy val IsFunctionProp = new SystemProperty(IsFunctionOID, YesNoType, ExactlyOne,
    toProps(
      setName("Is Function"),
      SkillLevel(SkillLevelAdvanced),
      Summary("True iff this Thing is a Function."),
      Details("""This is a marker flag that you can put on a Thing to say that it is a Function.
          |This doesn't particularly change the way the Thing works, but has some UI effects.""".stripMargin)))

	
  // TODO: Rework _hasProperty to be smarter about DWIMming its parameter. Introduce
  // inv.processParamPropertyRef(), which expects the specified parameter to be a Property.
  // It should try evaluating the param -- if the result is a Property, return that; if not,
  // and the parameter is a Property itself, return that. That is, let's get rid of the
  // need for the _self by returning the fully-evaluated parameter only if it actually
  // results in a Property. That allows indirections to work, but the common case of
  // an explicitly-named Property to *also* work.
  lazy val HasPropertyMethod = new InternalMethod(HasPropertyMethodOID,
    toProps(
      setName("_hasProperty"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Allows you to test whether this Thing has a specified Property"),
      Details("""    THING -> _hasProperty(PROP._self) -> Yes or No
    |
    |This function produces Yes iff the parameter is a Property of the received THING, and No otherwise.
    |Note that you must specify _self on the Property's name -- the parameter is the Property itself,
    |not its value on this Thing.""".stripMargin)))
  { 
	override def qlApply(inv:Invocation):QValue = {
	  for (
	    thing <- inv.contextAllThings;
	    propOid <- inv.processParamFirstAs(0, LinkType)
	      )
	    yield ExactlyOne(thing.props.contains(propOid))
	}
  }


  lazy val AllThingsMethod = new SingleThingMethod(AllThingsOID, "_allThings", "This receives a Space, and produces all of the Things in that Space",
	    """    SPACE -> _allThings -> LIST OF THINGS""".stripMargin,
	{ (thing, context) =>
	  thing match {
	    case s:SpaceState => Core.listFrom(s.allThings.map(_.id), LinkType)
	    case _ => QL.WarningValue("_allThings can only be used on a Space")
	  }
	})
  
  lazy val AsTypeMethod = new InternalMethod(AsTypeMethodOID,
    toProps(
      setName("_asType"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Convert one or more values of one Type to another Type"),
      Details("""This will not work for every possible conversion -- at the moment, it depends on whether
          |the two types serialize in compatible ways. But it works in many cases.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      implicit val s = inv.state
      for {
        newTypeId <- inv.processParamFirstAs(0, LinkType)
        newType <- inv.opt(inv.state.typ(newTypeId))
        vContext <- inv.contextElements
        qv = vContext.value
        ser = qv.pType.serialize(qv.first)
        newVal = newType.deserialize(ser)
      }
        yield ExactlyOne(newVal)
    }
  }
  
  lazy val ModelFunction = new InternalMethod(ModelFunctionOID,
    toProps(
      setName("_model"),
      Summary("Produces the Model of the received Thing")))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        thing <- inv.contextAllThings
      }
        yield ExactlyOne(LinkType(thing.model))
    }
  }

  override lazy val props = Seq(
    CopyIntoInstances,
      
    new InstancesMethod,
    new RefsMethod,
    new SpaceMethod,
    new ExternalRootsMethod,
    new AllPropsMethod,
    AllTypesMethod,
    new ChildrenMethod,
    new IsModelMethod,
    new IsDefinedMethod,
    new OIDMethod,
    new KindMethod,
    new CurrentSpaceMethod,
    new IsMethod,
    PropsOfTypeMethod,
    IsFunctionProp,
    HasPropertyMethod,
    AllThingsMethod,
    AsTypeMethod,
    ModelFunction
  )
}