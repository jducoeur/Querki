package querki.datamodel

import models.{Kind, PType}
import querki.ecology._
import querki.globals._
import querki.ql.{InvocationValue, QLCall, QLExp}
import querki.spaces.{TCRReq, ThingChangeRequest}
import querki.util.{PublicException, Contributor, Publisher}
import querki.values._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedSet

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
  val OrphanedInstancesOID = moid(7)
  val IsAFunctionOID = moid(8)
  val UsingSpaceOID = moid(9)
  val AllRefsOID = moid(10)
  val WithValueInOID = moid(11)
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
  
  lazy val Apps = interface[querki.apps.Apps]
  lazy val QL = interface[querki.ql.QL]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  override def init = {
    SpaceChangeManager.thingChanges += PropCopier
  }
  
  override def term = {
    SpaceChangeManager.thingChanges += PropCopier
  }
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Links = interface[querki.links.Links]
  lazy val PropPaths = interface[querki.types.PropPaths]
  lazy val Tags = interface[querki.tags.Tags]
  
  private object PropCopier extends Contributor[TCRReq, TCRReq] {
    // This is called whenever we get a Create or Modify request.
    def notify(evtReq:TCRReq, sender:Publisher[TCRReq,TCRReq]):TCRReq = {
      evtReq.map { evt =>
        evt match {
          // We only worry about it if there is a Model and there isn't a Thing, so this is a Create:
          case ThingChangeRequest(who, req, state, router, Some(modelId), None, kind, props, changedProps) => {
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
                
                ThingChangeRequest(who, req, state, router, Some(modelId), None, kind, newProps, changedProps)
              }
              // TODO: this is actually a strange error -- what should we do here?
              case None => evt
            }
          }
          case _ => evt
        }
      }
    }
  }
  
  def isDeletable(t:Thing, allowIfProp:Boolean = false)(implicit state:SpaceState):Boolean = {
    t match {
      case thing:ThingState => true
      case prop:Property[_,_] => {
        allowIfProp || {
          // You are allowed to delete a Property *if* nothing is using it any more:
          val thingsWithProp = state.localThings.filter(_.props.contains(prop.id))
          thingsWithProp.isEmpty
        }
      }
      case _ => false
    }
  }
  
  def getDeletedValue(prop:AnyProp)(implicit state:SpaceState):QValue =  
    new QValue {
      override def isDeleted = true
      
      val cType = prop.cType
      // TODO: why are these casts necessary? Something's wrong here...
      val cv = prop.default.cv.asInstanceOf[cType]
      def pType = prop.pType.asInstanceOf[PType[_]]
      
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
      Categories(DataModelTag),
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
      Categories(DataModelTag),
      Summary("Returns all of the non-Model Things that are based on this"),
      Signature(
        expected = Some(Seq(LinkType), "A Model"),
        reqs = Seq.empty,
        opts = Seq(
          ("useAppModel", YesNoType, ExactlyOne(true), "Iff true, and this Model is from an App, this returns all the Things based on the App's version of it.")
        ),
        returns = (LinkType, "All of the Instances based on this Model")
      ),
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
          |If your Instances have Names or Link Names, this list will be sorted by those names.
          |
          |If you have sub-Models under *Model* (that add more Properties, for example), this will include Instance of those as well.
          |
          |This will usually include Instances found in Apps, if there are any. (There usually aren't, but it's sometimes
          |relevant.) If you only want Instances of the local Shadow Model, set useAppModel to false.""".stripMargin)))
  {
    def getAppModels(rootsIn:Seq[OID], state:SpaceState):Seq[OID] = {
      rootsIn.map { rootId =>
        state.anything(rootId) match {
          case Some(root) => Apps.getShadowedThing(root)(state).id
          case _ => rootId
        }
      }      
    }
    
    override def qlApply(invIn:Invocation):QFut = {
      val inv = invIn.preferDefiningContext
      for {
        useAppModel <- inv.processAs("useAppModel", YesNoType)
        // We intentionally don't iterate in the usual way, because we want to combine the values. Note
        // that descendants() returns SortedSets, which can be combined to preserve the sorting. The below
        // logic is designed specifically to preserve that sorting until we're done.
        context = inv.context
        _ <- inv.test(context.value.matchesType(LinkType), "Func.notThing", List(displayName))
        rootsIn = context.value.rawList(LinkType)
        _ <- inv.test(!rootsIn.isEmpty, "Func.missingReceivedValue", List(displayName))
        roots = {
          if (useAppModel) {
            getAppModels(rootsIn, inv.state)
          } else
            rootsIn
        }
        allSets = roots.map(root => inv.state.descendants(root, false, true, true))
        // At this point, we have to lock it down into sequence, since now we will transform it to OIDs:
        descendants = allSets.reduce(_ ++ _).toSeq
      }
        yield QList.makePropValue(descendants.map(desc => LinkType(desc)), LinkType)
    }
  }
  
  // The Links side of _refs, which gets combined with the Tags side:
  def refs(inv:Invocation):QFut = {
    for {
      dummy <- inv.preferCollection(QList)
      definingProp <- inv.definingContextAsOptionalPropertyOf(LinkType)
      // Note that we presume that the received list is all of the same Model:
      firstThing <- inv.contextFirstThing
      linkProps = definingProp match {
        case Some(prop) => List(prop)
        case _ => {
          val modelOID = firstThing.model
          
          inv.state.
          propList.
          filter(prop => prop.pType == LinkType).
          // filter this to just the Properties with the right Restricted To:
          filter(prop => prop.getPropVal(Links.LinkModelProp)(inv.state).contains(LinkType, modelOID)).
          map(_.confirmType(LinkType).get)
        }
      }
      prop <- inv.iter(linkProps)
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
  
  class RefsMethod extends InternalMethod(RefsMethodOID, 
    toProps(
      setName("_refs"),
      Categories(DataModelTag),
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
          |Note that this always returns a List, since any number of Things could be pointing to this.
          |
          |**Note:** `_refs` only produces the "hard" references, through Thing Properties. Most of the
          |time, you should use `_allRefs` instead -- that includes references through Tags as well.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      refs(inv)
    }
  }
  
  lazy val AllRefsMethod = new InternalMethod(AllRefsOID,
    toProps(
      setName("_allRefs"),
      Categories(DataModelTag),
      Summary("""Produces all references to this Thing, whether in Thing or Tag Properties"""),
      Signature(
        expected = Some((Seq(LinkType), "The Thing to fetch the references to")),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (LinkType, "The Things that refer to the received Thing")),
      Details("""`_allRefs` is more or less exactly the combination of `_refs` and `_tagRefs`. It is
        |generally the function to use unless you specifically want just Thing or Tag Properties.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        linkRefs <- refs(inv)
        tagRefs <- Tags.tagRefs(inv)
        allOIDs = linkRefs.rawList(LinkType) ++ tagRefs.rawList(LinkType)
        allElems = allOIDs.map(LinkType(_))
      }
        yield QList.makePropValue(allElems, LinkType)
    }
  }
  
  lazy val UsingSpace = new InternalMethod(UsingSpaceOID, 
    toProps(
      setName("_usingSpace"),
      Categories(DataModelTag),
      Summary("Sets the Space to use for the rest of this QL phrase"),
      SkillLevel(SkillLevelAdvanced),
      Signature(
        expected = None,
        reqs = Seq(("space", LinkType, "The Space to use")),
        opts = Seq.empty,
        returns = (AnyType, "The original received value")
      ),
      Details("""When doing high-level meta-programming in Querki, you may find that you want to do
        |something not in the context of the current Space, but with one of its Apps instead. In this
        |case, the `_usingSpace` function allows you to switch to that App for the remainder of this
        |phrase.""".stripMargin)))
  {
    override def qlApplyTop(inv:Invocation, transformThing:Thing):Future[QLContext] = {
      val result = for {
        appId <- inv.processAs("space", LinkType)
        state <- inv.opt(inv.state.getApp(appId))
      }
        yield state
        
      result.get.map { it =>
        if (it.isEmpty)
          throw new PublicException("DataModel.usingSpace.notAnApp")
        else
          inv.context.copy()(it.head, ecology)
      }
    }
  }
  
  class SpaceMethod extends InternalMethod(SpaceMethodOID, 
    toProps(
      setName("_space"), 
      Categories(DataModelTag),
      Summary("What Space is this Thing in?"), 
      Details("""```
        |RECEIVED -> _space -> SPACE
        |```
        |
        |This function produces the Space that the received Thing is contained in.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextFirstThing
      }
        yield Links.LinkValue(thing.spaceId)
    }
  }

  class ExternalRootsMethod extends InternalMethod(ExternalRootsOID, 
    toProps(
      setName("_externalRoots"), 
      Categories(DataModelTag),
      Summary("What are the ancestor Things for this Space?"), 
      Details("""```
        |SPACE -> _externalRoots -> ROOTS
        |```
        |
        |Pass in a link to a Space; this produces all of the "roots" -- the Things from its Apps -- used
        |by that Space.
        |
        |User code will rarely care about this function, but it is part of how the [[All Things._self]] display works.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val state = inv.state
      for {
        thing <- inv.contextFirstThing
        thingRoots = {
          ((Set.empty[OID] /: state.localThings) ((set, t) => set + state.root(t))).
            filterNot(oid => state.anything(oid).map(_.ifSet(Core.InternalProp)).getOrElse(false))
        }
      }
        yield Core.listFrom(thingRoots, LinkType)
    }
  }

  class AllPropsMethod extends InternalMethod(AllPropsMethodOID, 
    toProps(
      setName("_allProps"), 
      Categories(DataModelTag),
      Summary("What are the Properties in this Space?"), 
      Details("""```
        |SPACE -> _allProps -> PROPS
        |```
        |
        |This receives a link to a Space, and produces all of the Properties defined in that Space.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextFirstThing
      }
        yield {
          thing match {
            case s:SpaceState => Core.listFrom(s.propList.toSeq.sortBy(_.displayName), Core.LinkFromThingBuilder) 
            case _ => QL.WarningValue("_allProps must be used with a Space")
          }
        }
    }
  }

  lazy val AllTypesMethod = new InternalMethod(AllTypesMethodOID, 
    toProps(
      setName("_allTypes"), 
      Categories(DataModelTag),
      Summary("What are the Types in this Space?"), 
      Details("""```
        |SPACE -> _allTypes -> TYPES
        |```
        |
        |This receives a link to a Space, and produces all of the Types defined in that Space.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextFirstThing
      }
        yield {
          thing match {
            case s:SpaceState => Core.listFrom(s.types.values.toSeq.sortBy(_.displayName), Core.LinkFromThingBuilder) 
            case _ => QL.WarningValue("_allTypes must receive a Space")
          }
        }
    }
  }
  
  class ChildrenMethod extends InternalMethod(ChildrenMethodOID,
    toProps(
      setName("_children"),
      Categories(DataModelTag),
      Summary("This produces the immediate children of the received Model."),
      Signature(
        expected = Some(Seq(LinkType), "A Model"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (LinkType, "All of the direct children of that Model in that Space. This may include sub-Models, if there are any, as well as Instances.")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        model <- inv.contextAllThings
        child <- inv.iter(inv.state.allChildren(model))
      }
        yield ExactlyOne(LinkType(child))
    }
  }

  class IsModelMethod extends InternalMethod(IsModelMethodOID, 
    toProps(
      setName("_isModel"), 
      Categories(DataModelTag),
      Summary("This produces Yes if the received Thing is a Model."),
      Details("""    THING -> _isModel -> Yes or No""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
      }
        yield ExactlyOne(thing.isModel(inv.state))
    }
  }

  class IsDefinedMethod extends InternalMethod(IsDefinedOID,
    toProps(
      setName("_isDefined"),
      Categories(DataModelTag),
      Summary("Produces Yes if the name passed into it is a real Thing"),
      Details("""    NAME -> _isDefined -> YES or NO
          |You typically use _isDefined with a Tag Property. It is simply a way to ask "is there actually something
          |with this name?", so that you can handle it differently depending on whether there is or not.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
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
      else if (qv.pType.isInstanceOf[querki.core.IsLinkType])
        state.anything(qv.firstAs(LinkType).get).isDefined
      else
        false
    }
  }

  class OIDMethod extends InternalMethod(OIDMethodOID, 
    toProps(
      setName("_oid"), 
      Categories(DataModelTag),
      Summary("Get the unique global id of this Thing"), 
      Details("""```
        |THING -> _oid -> Text
        |```
        |
        |This function produces the unique Object ID (which will generally be a period followed by some letters and numbers)
        |of the received Thing.
        |
        |Each Thing in Querki has an Object ID. In most cases, it can be used in place of the Thing's name, and it is never
        |ambiguous -- it always refers to one specific Thing.
        |
        |**Advanced:** This can be used to print out OIDs that do *not* correspond to Things in this Space. This is
        |rarely useful, but is useful for a few very advanced situations.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        oid <- inv.contextAllAs(LinkType)
      }
        yield Basic.TextValue(oid.toThingId)
    }
  }

  class KindMethod extends InternalMethod(KindMethodOID,
    toProps(
      setName("_kind"), 
      Categories(DataModelTag),
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
    override def qlApply(inv:Invocation):QFut = {
      inv.paramsOpt match {
        // First version of this function: if there is a parameter, it should be the name of a Kind:
        // TODO: rewrite this horror:
        case Some(params) => {
          val param = params(0);
          val QLCall(kindName, _, _, _) = param.exp.phrases.head.ops(0)
          Future.successful(Kind.fromName(kindName.name).map(kind => ExactlyOne(IntType(kind))).getOrElse(QL.WarningValue("Unknown Kind: " + kindName)))
        }
      
        // Second version: return the Kind of the received value:
        case None => {
          for {
            thing <- inv.contextAllThings
          }
            yield ExactlyOne(IntType(thing.kind))
        }
      }
    }
  }

  class CurrentSpaceMethod extends InternalMethod(CurrentSpaceMethodOID, 
    toProps(
      setName("_currentSpace"), 
      Categories(DataModelTag),
      Summary("What Space are we looking at?"), 
      Details("""```
        |_currentSpace -> SPACE
        |```
        |
        |This function produces the Space that we are currently displaying. (Generally, the one in the URL.)""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      fut(Links.LinkValue(inv.context.root.state))
    }
  }

  class IsMethod extends InternalMethod(IsMethodOID,
    toProps(
      setName("_is"),
      Categories(DataModelTag),
      Summary("Allows you to test whether you have a specific Thing"),
      Details("""    THING -> _is(THING) -> Yes or No
    |
    |This function produces Yes iff the parameter matches the passed-in THING, and No otherwise. It is almost always used
    |inside _if(). For instance, to check whether a Property is of Text Type:
    |    MyProp.Property Type -> _if(_is(Text Type), ...)""".stripMargin)))
  { 
    override def qlApply(inv:Invocation):QFut = {
      for 
      (
        receivedId <- inv.contextAllAs(LinkType);
        paramId <- inv.processParamFirstAs(0, LinkType)
      )
        yield ExactlyOne(receivedId == paramId)
    }
  }
  
  lazy val IsAFunction = new InternalMethod(IsAFunctionOID,
    toProps(
      setName("_isA"),
      Categories(DataModelTag),
      Summary("Allows you to test whether this Thing is descended from a given Model"),
      Signature(
        expected = Some(Seq(LinkType), "A Thing - either a Model or an Instance"),
        reqs = Seq(
          ("model", LinkType, "The Model that this might be descended from.")
        ),
        opts = Seq.empty,
        returns = (YesNoType, "True iff the Thing is descended from the Model, false otherwise")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
        modelId <- inv.processAs("model", LinkType)
      }
        yield ExactlyOne(thing.isAncestor(modelId)(inv.state))
    }
  }
  
  lazy val PropsOfTypeMethod = new InternalMethod(PropsOfTypeOID,
    toProps(
      setName("_propsOfType"),
      Categories(DataModelTag),
      Summary("This receives a Type, and produces all of the Properties in this Space with that Type"),
      Signature(
        expected = Some(Seq(LinkType), "A Type"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (LinkType, "All of the Properties of that Type, in that Space.")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
        // The next two lines are ugly. We might add a match-like function to inv?
        dummy <- inv.test(thing.isInstanceOf[PType[_]], "DataModel.propsOfType.notAType", thing.displayName)
        tpe = thing.asInstanceOf[PType[_]]
        prop <- inv.iter(inv.state.propsOfType(tpe))
      }
        yield ExactlyOne(LinkType(prop))
    }
  }

  lazy val IsFunctionProp = new SystemProperty(IsFunctionOID, YesNoType, ExactlyOne,
    toProps(
      setName("Is Function"),
      Categories(DataModelTag),
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
      Categories(DataModelTag),
      SkillLevel(SkillLevelAdvanced),
      Summary("Allows you to test whether this Thing has a specified Property"),
      Details("""```
    |THING -> _hasProperty(PROP._self) -> Yes or No
    |```
    |
    |This function produces Yes iff the parameter is a Property of the received THING, and No otherwise.
    |Note that you must specify _self on the Property's name -- the parameter is the Property itself,
    |not its value on this Thing.""".stripMargin)))
  { 
  	override def qlApply(inv:Invocation):QFut = {
  	  for {
  	    thing <- inv.contextAllBundles
  	    propOid <- inv.processParamFirstAs(0, LinkType)
      }
  	    yield ExactlyOne(thing.props.contains(propOid))
  	}
  }


  lazy val AllThingsMethod = new InternalMethod(AllThingsOID, 
    toProps(
      setName("_allThings"), 
      Categories(DataModelTag),
      Summary("This receives a Space, and produces all of the Things in that Space"),
	    Details("""```
        |SPACE -> _allThings -> LIST OF THINGS
        |```""".stripMargin)))
	{ 
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
      }
        yield {
          thing match {
            case s:SpaceState => Core.listFrom(s.localThings.map(_.id), LinkType)
            case _ => QL.WarningValue("_allThings can only be used on a Space")
          }          
        }
    }
  }
  
  lazy val AsTypeMethod = new InternalMethod(AsTypeMethodOID,
    toProps(
      setName("_asType"),
      Categories(DataModelTag),
      SkillLevel(SkillLevelAdvanced),
      Summary("Convert one or more values of one Type to another Type"),
      Details("""This will not work for every possible conversion -- at the moment, it depends on whether
          |the two types serialize in compatible ways. But it works in many cases.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
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
      Categories(DataModelTag),
      Summary("Produces the Model of the received Thing")))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
      }
        yield ExactlyOne(LinkType(thing.model))
    }
  }
  
  lazy val OrphanedInstancesFunction = new InternalMethod(OrphanedInstancesOID,
    toProps(
      setName("_orphanedInstances"),
      Categories(DataModelTag),
      SkillLevel(SkillLevelAdvanced),
      Summary("Lists all of the Instances in this Space (if any) whose Models have been deleted"),
      Details("""The results of this are available from the Advanced page, so you probably will never
        |need to use this function manually.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val s = inv.state
      for {
        t <- inv.iter(s.localThings)
        if (t.hasModel && s.anything(t.model).isEmpty)
      }
        yield ExactlyOne(LinkType(t))
    }
  }

  lazy val instanceValueCacheKey = StateCacheKey(21, "instanceValues")

  /**
    * This is the heart of the optimization of _withValueIn: it is a cached map of values to the instances that
    * contain them.
    */
  type InstanceValueMap[VT] = Map[VT, Set[OID]]

  lazy val WithValueInMethod = new InternalMethod(WithValueInOID,
    toProps(
      setName("_withValueIn"),
      Summary("Produces the instances of this Model that have the specified value"),
      Signature(
        expected = Some(Seq(AnyType), "One or more values"),
        reqs = Seq(("exp", AnyType, "An expression to apply to the instances of the model")),
        opts = Seq.empty,
        returns = (AnyType, "The instances of this Model that have the given value in the given property")
      ),
      Details(
        """
          |In serious Querki spaces, you will sometimes find that you want to do something like this this:
          |```
          |value -> +$v
          |model._instances -> _filter(property -> _equals($v))
          |```
          |That is, you want to collect all of the instances of the model with the given value.
          |
          |The problem is, the above code is a bit clunky; worse, it's quite slow. This isn't a huge problem when
          |you are only doing it occasionally, for models with modest numbers of instances. But if you are doing it
          |a lot, and you have thousands of instances, it can make pages unusably slow.
          |
          |This comes up particularly often when representing trees of data in Querki -- where you have "child"
          |things pointing to "parent" ones. When you want to draw this, each parent needs to find all of the
          |instances that point to it, recursively, all the way down. With just a few hundred instances, this can
          |destroy a page.
          |
          |So this convenience function does exactly the same thing, but in a way that is easier to read and write
          |and is, most importantly, a bazillion times faster. (The important bit is that this caches its results.
          |So while it only runs somewhat faster the first time, it can run orders of magnitude faster if you
          |do it repeatedly.)
          |
          |So when you need to do this sort of thing, reach
          |for this function: your pages will work better, and it's better for Querki overall.
          |
          |The syntax is:
          |```
          |value -> model._withValueIn(property)
          |```
          |
          |You can pass a collection of values into this; if so, all Things that match any of them will be produced.
          |
          |You aren't actually limited to a simple property as the parameter -- you can put any QL expression there,
          |and it will be tried with all instances of the model.
          |
          |The property or expression may be a collection rather than a single value. If so, this will produces all
          |of the instances where *any* of the received values match *any* of the results of the expression.
          |
          |For the moment, this is only really tested with Thing Type properties. It may work with others, but it
          |certainly needs more work. If you have another type that isn't working, which would be really useful for
          |you, please drop us a line!
        """.stripMargin)
    ))
  {
    /**
      * This fetches (or creates) the dynacache table for the ValueMaps.
      *
      * This is a pretty complex table. The key is model/expression pairs; the value is the table of values, and
      * which instances contain each value. Note that the value is a Future, so
      * that we don't have to stop and block anywhere inconveniently stupid. It is specifically *not* an
      * InvocationValue -- we tried that, and discovered that InvocationValue really isn't designed to be
      * long-lived. (For example, it checks for timeouts in its .flatMap().)
      */
    def getValueMaps[VT](inv: Invocation): TrieMap[(OID, QLExp), Future[InstanceValueMap[VT]]] = {
      // Note that this plays a little fast and loose with the dynacache: VT varies from key to key. This only works
      // because fetchOrCreateCache itself cheats.
      inv.state.fetchOrCreateCache(instanceValueCacheKey, TrieMap.empty[(OID, QLExp), Future[InstanceValueMap[VT]]])
    }

    /**
      * For a given Model, this evaluates the given expression over each instance of the model, and builds it into an
      * InstanceValueMap.
      */
    def createValueMap[VT](inv: Invocation, modelId: OID, exp: QLExp, expectedType: PType[VT]): Future[InstanceValueMap[VT]] = {
      implicit val rc = inv.context.request
      implicit val state = inv.state

      val instances: SortedSet[Thing] = inv.state.descendants(modelId, includeModels = false, includeInstances = true)
      val instanceMap: Future[InstanceValueMap[VT]] =
        instances.foldLeft(Future.successful(Map.empty[VT, Set[OID]])) { (minv, thing) =>
          minv.flatMap { m =>
            val context = thing.thisAsContext
            inv.process("exp", context).get.map { qv =>
              val vs: List[VT] = qv.head.rawList(expectedType)
              vs.foldLeft(m) { (nextm, v) =>
                val oids = nextm.get(v).getOrElse(Set.empty[OID])
                nextm + (v -> (oids + thing.id))
              }
            }
          }
        }
      instanceMap
    }

    /**
      * This tries to fetch the caches ValueMap for the given Model/Exp pair. If not found, it creates it.
      */
    def getValueMap[VT](inv: Invocation, modelId: OID, exp: QLExp, expectedType: PType[VT]): Future[InstanceValueMap[VT]] = {
      getValueMaps[VT](inv).getOrElseUpdate((modelId, exp), createValueMap(inv, modelId, exp, expectedType))
    }

    override def qlApply(inv: Invocation): QFut = {
      for {
        // TODO: give a reasonable error if there is no defining context
        definingContext <- inv.opt(inv.definingContext)
        // TODO: give a reasonable error if the defining context isn't a thing
        if (definingContext.value.pType == LinkType)
        modelId = definingContext.value.rawList(LinkType).head
        // TODO: is the below going to parallelize on the context elements? We actually don't want that.
        contextElem <- inv.contextElements
        pt = contextElem.value.pType
        exp <- inv.rawRequiredParam("exp")
        maps = getValueMaps(inv)
        valueMap <- inv.fut(getValueMap(inv, modelId, exp, pt))
        vOpt = contextElem.value.rawList(pt).headOption
        if (vOpt.isDefined)
        v = vOpt.get
        instances: Set[OID] = valueMap.get(v).getOrElse(Set.empty)
        instanceId <- inv.iter(instances)
      }
        yield ExactlyOne(LinkType(instanceId))
    }
  }

  override lazy val props = Seq(
    CopyIntoInstances,
      
    new InstancesMethod,
    new RefsMethod,
    AllRefsMethod,
    UsingSpace,
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
    IsAFunction,
    HasPropertyMethod,
    AllThingsMethod,
    AsTypeMethod,
    ModelFunction,
    OrphanedInstancesFunction,
    WithValueInMethod
  )
}