package querki.core

import models._

import querki.api.commonName
import querki.basic.PlainTextBaseType
import querki.conventions
import querki.ecology._
import querki.globals._
import querki.values.{ElemValue, PropAndVal, QLContext, QValue, SpaceState}

class CoreModule(e:Ecology) extends CoreEcot(e) with Core with WithQL
  with CollectionBase with CollectionCreation with CoreExtra
  with TextTypeBasis with IntTypeBasis with LinkUtils with NameUtils with NameTypeBasis with PlainTextBaseType 
  with TypeCreation
{
  import MOIDs._
  
  lazy val Links = interface[querki.links.Links]
  lazy val QL = interface[querki.ql.QL]
  
  def LinkKindProp(kind:Kind.Kind) = (querki.links.PublicMOIDs.LinkKindOID -> ExactlyOne(IntType(kind)))
  def LinkAllowAppsProp(b:Boolean) = (querki.links.PublicMOIDs.LinkAllowAppsOID -> ExactlyOne(YesNoType(b)))
  
  /**
   * The Ur-Thing, from which the entire world descends. Note that this is
   * its own parent!
   */
  lazy val UrThing = new ThingState(RootOID, systemOID, RootOID,
    toProps(
      setName("Ur Thing"),
      // TODO: once we rework the UI some more, we probably can and should remove this Optional from here.
      // It is really only here to remind the Space author to think about whether something is a Model.
      (querki.core.MOIDs.IsModelOID -> ExactlyOne(ElemValue(false, new DelegatingType(YesNoType))))
      ))
  {
    override def getProp(propId:OID)(implicit state:SpaceState):PropAndVal[_] = {
      // If we've gotten up to here and haven't found the property, use
      // the default:
      localOrDefault(propId)
    }
    
    override def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):QValue = {
      localPropVal(prop).getOrElse(prop.default)
    }
  
    override def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
      props.contains(propId)
    }
    
    override def allProps(implicit state:SpaceState):Set[Property[_,_]] = localProps
  
    override def hasModel = false
    
    override def thingOps(e:Ecology) = new ThingOps(this) {
      override def isAncestor(other:OID)(implicit state:SpaceState):Boolean = false    
    }
  }
  
  override lazy val things = Seq(
    UrThing
  )

  /***********************************************
   * COLLECTIONS
   * 
   * These are all defined in the Collections file.
   ***********************************************/
  
  lazy val UrCollection = new UrCollection
  lazy val ExactlyOne = new ExactlyOne
  lazy val Optional = new Optional
  lazy val QList = new QList
  lazy val QSet = new QSet
  lazy val QUnit = new QUnit
  lazy val bootCollection = new bootCollection
  
  lazy val QNone = Optional.QNone
  def emptyOpt(pType:PType[_]) = Optional.Empty(pType)
  def listFrom[RT,VT](in:Iterable[RT], builder:PTypeBuilderBase[VT,RT]):QValue = QList.from(in, builder)
  def makeListValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = QList.makePropValue(cv, elemT)
  def makeSetValue(rawList:Seq[ElemValue], pt:PType[_], context:QLContext):QValue = QSet.makeSetValue(rawList, pt, context)
  
  // In practice, system-set Names use the bootCollection instead of ExactlyOne, so that we don't have
  // dependency problems.
  def setName(v:String) = (NameOID -> bootCollection(ElemValue(v, new DelegatingType({NameType}))))

  // PRIVATE: this is the workaround so we can mark key boot-critical Things as Internal while avoiding
  // initialization loops.
  def setInternal = (InternalPropOID -> bootCollection(ElemValue(true, new DelegatingType({YesNoType}))))
  
  override lazy val colls = Seq(
    UrCollection,
    ExactlyOne,
    Optional,
    QList,
    QSet,
    QUnit
  )
  
  def emptyListOf(pType:PType[_]) = QList.empty(pType)
  def emptyList = QList.empty

  /***********************************************
   * TYPES
   * 
   * These are all defined in the Types file.
   ***********************************************/
  
  lazy val UnknownType = new UnknownType
  lazy val UrType = new UrType
  lazy val InternalMethodType = new InternalMethodType
  lazy val TextType = new TextType
  lazy val LargeTextType = new LargeTextType
  lazy val LinkType = new LinkType
  lazy val NameType = new NameType
  lazy val IntType = new IntType
  lazy val YesNoType = new YesNoType
  lazy val LongType = new LongType
  lazy val FloatType = new FloatType
  lazy val TagType = new TagType
  
  lazy val LinkFromThingBuilder = new PTypeBuilderBase[OID, Thing] {
    def pType = LinkType
    def wrap(raw:Thing):OID = raw.id
  }
  
  def followLink(context:QLContext):Option[Thing] = LinkType.followLink(context)
  
  override lazy val types = Seq(
    UrType,
    InternalMethodType,
    TextType,
    LargeTextType,
    LinkType,
    NameType,
    IntType,
    YesNoType,
    LongType,
    FloatType,
    TagType
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  // Our own Core-safe versions of a few QuerkiEcot shadows:
  def Summary(text:String) = (querki.conventions.MOIDs.PropSummaryOID -> ExactlyOne(TextType(text)))
  def Details(text:String) = (querki.conventions.MOIDs.PropDetailsOID -> ExactlyOne(LargeTextType(text))) 
  def Categories(cats:String*) = (querki.conventions.MOIDs.PropCategoriesOID -> QSet.makePropValue(cats.map(TagType(_)), TagType))
  def NotInherited = (querki.core.MOIDs.NotInheritedOID -> ExactlyOne(YesNoType(true)))
  lazy val SkillLevelBasic = querki.identity.skilllevel.MOIDs.SkillLevelBasicOID
  lazy val SkillLevelStandard = querki.identity.skilllevel.MOIDs.SkillLevelStandardOID
  lazy val SkillLevelAdvanced = querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID
  def SkillLevel(level:OID) = (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(LinkType(level)))

  /**
   * The root Property, from which all others derive.
   */
  lazy val UrProp = Property(UrPropOID, systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName(commonName(_.core.urProp)),
        setInternal,
        (conventions.MOIDs.PropSummaryOID -> Optional.QNone),
        (querki.basic.MOIDs.DisplayTextOID -> Optional(LargeTextType("""[[Summary -> ""**____** -- ""]]
            |[[_if(Property Type -> _is(Internal Method Type), 
            |  ""**Function**"",
            |  ""Collection: [[Property Collection]] Type: [[Property Type]]"")]]
            |  
            |[[_if(Is User Value Property, ""**User Value Property** -- has a different value for each User"")]]
            |  
            |[[_if(_and(Property Type -> _is(Internal Method Type), _isNonEmpty(_Function Signature)), 
            |""{{well well-sm:
            |**[[_Display Function Signature]]**
            |
            |[[_Display Function Parameters]]
            |}}"")]]
            |
            |[[Details]]
            |
            |[[_if(_and(Is User Value Property, _hasProperty(Summary Link._self)), ""If this Property was recently modified, the Summaries may be out of sync.
            |Press this button to make sure that they are correctly calculated.
            |
            |[[//_QLButton(""Recalculate Summaries"", _updatePropSumaries, ""_recalcResult"")]]<div id="_recalcResult"></div>"")]]
            |
            |[[_if(_not(Property Type -> _is(Internal Method Type)), 
            |""#### Things that use ____
            |
            |[[_currentSpace ->
            |  _allThings ->
            |  _filter(_hasProperty($_context._self)) ->
            |  _sort ->
            |  _bulleted]]"")]]
            |""".stripMargin)))
        ), querki.time.epoch)

  lazy val NotInheritedProp = new SystemProperty(NotInheritedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Not Inherited"),
      // Need to define this explicitly, to break infinite loops in lookup:
      (NotInheritedOID -> ExactlyOne(querki.values.ElemValue(false, new DelegatingType({YesNoType})))),
      // For reasons I really don't understand, we need to set this explicitly:
      (InternalPropOID -> bootCollection(ElemValue(false, new DelegatingType({YesNoType})))),
      AppliesToKindProp(Kind.Property),
      SkillLevel(SkillLevelAdvanced),
      Categories(CoreTag),
      Summary("Should this Property be inherited from ancestors?"),
      Details("""All Things in Querki are part of a big inheritance tree. Instances inherit from their Models,
          |which in turn usually inherit from higher-level Models.
          |
          |When we say that they "inherit", we mean that the lower-level Things (the Instances) pick up default
          |values of their Properties from their higher-level ancestors (the Models). So for instance, if the Model
          |defines a Default View, the Instance will use that Default View unless it redefines it.
          |
          |This is almost always the right way for things to work, but a very few Properties are different: they
          |must be defined on each individual Thing they apply to, and should *not* be inherited. For example, the
          |*Is a Model* flag must not be inherited, because if it was, then all the Instances of a Model would
          |themselves be marked as Models.
          |
          |So you can use the *Not Inherited* flag to indicate that this Property should not be inherited from
          |Models to Things. This is a very advanced Property, and not intended for ordinary use.""".stripMargin)
      ))
  
  lazy val ModelOnlyProp = new SystemProperty(ModelOnlyPropOID, YesNoType, ExactlyOne,
    toProps(
      setName("Model Only Property"),
      SkillLevel(SkillLevelAdvanced),
      Categories(CoreTag),
      Summary("Set this flag on a Property if that Property should be defined *only* on the Model, not on Instances"),
      Details("""This is very advanced, and rarely useful in user code. In most cases, you should achieve the same
          |effect simply by setting the Instance Properties appropriately.""".stripMargin)))
  
  /**
   * The formal Name of a Thing. Note that system-created Names mostly use bootCollection instead of
   * ExactlyOne, to avoid initialization loops.
   */
  lazy val NameProp = new SystemProperty(NameOID, NameType, ExactlyOne,
    toProps(
      setName(commonName(_.core.nameProp)),
      NotInherited,
      Categories(CoreTag),
      Summary("The name used to point to this Thing"),
      Details("""The Link Name for a Thing is usually derived automatically from its normal Name.
          |It is the official identifier for that Thing, and is always unique within this Space.
          |So it is used, for instance, in the URL for this Thing, and it is the name you usually
          |use when referring to the Thing in QL, like \[[My Thing\]]. 
          |
          |Link Names should consist mainly of letters and numbers; they may also include dashes
      		|and spaces. They should usually start with a letter, and they shouldn't be
      		|excessively long.
          |
          |If the Thing's Name already follows these rules, and is unique within the Space, the
          |Link Name will be identical to the Name. If the Name contains illegal characters like
          |commas, quotes and so on, they will be removed when figuring out the Link Name. If the
          |result isn't unique within the Space, a number will be added to the end to make it unique.
          |
          |This usually works fine, but if you want to define your own Link Name for a Thing, rather
          |than letting it be derived automatically, uncheck the "Derive the Link Name from the Name" 
          |box in the Advanced Editor.""".stripMargin)
      ))

  /**
   * The Property that points from a Property to its Type.
   */
  lazy val TypeProp = new SystemProperty(TypePropOID, LinkType, ExactlyOne,
    toProps(
      setName(commonName(_.core.typeProp)),
      LinkKindProp(Kind.Type),
      LinkAllowAppsProp(true),
      AppliesToKindProp(Kind.Property),
      NotInherited,
      Categories(CoreTag),
      Summary("The Type that this Property can hold"),
      Details("""A Type is something like "Text" or "Number" or "Tag". Every Property must be designed for
      		|exactly one Type, and only values of that Type can be placed in it.
      		|
      		|Most Properties are of type Text or Large Text (which is the same thing, but can have multiple lines);
      		|if you don't know what to use, that's usually your best bet.""".stripMargin)
      ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  lazy val CollectionProp = new SystemProperty(CollectionPropOID, LinkType, ExactlyOne,
    toProps(
      setName(commonName(_.core.collectionProp)),
      LinkKindProp(Kind.Collection),
      LinkAllowAppsProp(true),
      AppliesToKindProp(Kind.Property),
      NotInherited,
      Categories(CoreTag),
      Summary("How much this Property holds"),
      Details("""Collection is a subtle but important concept. A Property Value isn't necessarily *one* of a
          |Type -- it can take several forms, and you have to say which one it is. As of this writing, the
          |available Collections are:
          |
          |* Required -- the most common case, where the Property holds one of this Type. Note that this means
          |    that it *always* holds one of this type; if you don't give it a value, it will be set to a default.
          |* Optional -- the Property may or may not hold a value of this Type. Use Optional when it makes sense
          |    for a Thing to have the Property but not have it filled in. It is very common for a Model to have an
          |    Optional Property, which its Instances can choose to fill in when it makes sense.
          |* List -- the Property holds an ordered list of this Type. You most often use List with Links. Note that
          |    you can rearrange the List using drag-and-drop, and duplicates are allowed.
          |* Set -- the Property holds an unordered set of this Type. You most often use Set with Tags. Duplicates
          |    will be silently thrown away, and order is not preserved; Sets are usually shown in alphabetical order.
          |
          |When you create a Property, you must choose which Collection it uses. If you aren't sure, it is usually
          |best to go for Optional, but you should use any of these when they make sense.""".stripMargin)
      ))

  /**
   * A flag set on a Thing to indicate that it should be used as a Model. Note that this
   * Property is not inherited: the child of a Model is not usually a Model.
   */
  lazy val IsModelProp = new SystemProperty(IsModelOID, YesNoType, ExactlyOne,
    toProps(
      setName(commonName(_.core.isModelProp)),
      NotInherited,
      setInternal,
      // TBD: we might allow Property Models down the road, but not yet:
      AppliesToKindProp(Kind.Thing),
      SkillLevel(SkillLevelAdvanced),
      Categories(CoreTag),
      Summary("Is this Thing a Model?"),
      Details("""All Things can be used as Models if you would like -- in Querki, unlike most programming
          |languages, the difference between a Model and an Instance is pretty small. But this flag indicates
          |that this Thing should *normally* be considered a Model. If you set it, then this Model will be
          |offered as a possibility when you go to create a Thing, it will be displayed a bit differently in
          |the Space's list of its Things, and in general will be handled a little differently.""".stripMargin)
      ))

  lazy val AppliesToKindProp = new SystemProperty(AppliesToKindOID, IntType, QList,
    toProps(
      setName("Applies To"),
      (AppliesToKindOID -> QList(ElemValue(Kind.Property, new DelegatingType(IntType)))),
      (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(LinkType(querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID))),
      Categories(CoreTag),
      Summary("Which Kinds of Things can this Property be used on?"),
      Details("""By default, a Property can be used on anything -- even when
          |that is nonsensical. The result is that, when creating a new Thing, you get a messy list of lots of
          |Properties, many of which are irrelevant.
          |
          |So to keep that from happening, use this on your Properties. In most cases, a Property is really intended
          |to only apply to Things *or* Properties, not both. So using this can keep you from having a long and
          |confusing Property List.
          |
          |This is an advanced Property, mostly because most user-defined Properties are intended to be used on
          |Things, and you usually want to see all of your user-defined Properties as options. But serious
          |programmers working in Querki may want to set this, especially if they are creating meta-Properties.""".stripMargin)
      ))

  lazy val InternalProp = new SystemProperty(InternalPropOID, YesNoType, Optional,
    toProps(
      setName("Internal Property"),
      AppliesToKindProp(Kind.Property),
      NotInherited,
      setInternal,
      Summary("If set, this Property is system-internal, and should not be visible to end users."),
      Details("""Pretty much by definition, you should never need to use this meta-Property.""".stripMargin)))

  override lazy val props = Seq(
    NotInheritedProp,
    UrProp,
    NameProp,
    TypeProp,
    CollectionProp,
    IsModelProp,
    AppliesToKindProp,
    InternalProp,
    ModelOnlyProp
  )
}
