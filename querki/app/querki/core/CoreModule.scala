package querki.core

import models._

import models.system.OIDs.{systemOID, DisplayTextOID}
import models.system.{IntType, LargeTextType, LinkType, NameType, QLType, TextType, YesNoType}

import querki.conventions
import querki.ecology._

import querki.values.{ElemValue, PropAndVal, QLContext, QValue, SpaceState}

class CoreModule(e:Ecology) extends CoreEcot(e) with Core {
  import MOIDs._
  
  def LinkKindProp(kind:Kind.Kind) = (querki.links.MOIDs.LinkKindOID -> ExactlyOne(IntType(kind)))
  def LinkAllowAppsProp(b:Boolean) = (querki.links.MOIDs.LinkAllowAppsOID -> ExactlyOne(YesNoType(b)))

  /**
   * The Ur-Thing, from which the entire world descends. Note that this is
   * its own parent!
   */
  lazy val UrThing = new ThingState(RootOID, systemOID, RootOID,
    toProps(
      setName("Thing"),
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
  
    override def isAncestor(other:OID)(implicit state:SpaceState):Boolean = false
  
    override def hasModel = false
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
  
  lazy val QNone = Optional.QNone
  def listFrom[RT,VT](in:Iterable[RT], builder:PTypeBuilderBase[VT,RT]):QValue = QList.from(in, builder)
  def makeListValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = QList.makePropValue(cv, elemT)
  def makeSetValue(rawList:Seq[ElemValue], pt:PType[_], context:QLContext):QValue = QSet.makeSetValue(rawList, pt, context)
  
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
   * PROPERTIES
   ***********************************************/
  
  // Our own Core-safe versions of a few QuerkiEcot shadows:
  def Summary(text:String) = (querki.conventions.MOIDs.PropSummaryOID -> ExactlyOne(TextType(text)))
  def Details(text:String) = (querki.conventions.MOIDs.PropDetailsOID -> ExactlyOne(LargeTextType(text))) 
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
        setName("Property"),
        (InternalPropOID -> ExactlyOne(YesNoType(true))),
        (conventions.MOIDs.PropSummaryOID -> Optional(TextType("The root Property, from which all others derive."))),
        (DisplayTextOID -> Optional(LargeTextType("""[[Summary -> ""**____** -- ""]]
            |[[_if(Property Type -> _is(Internal Method Type), 
            |  ""**METHOD**"",
            |  ""Collection: [[Property Collection]] Type: [[Property Type]]"")]]
            |
            |
            |[[Details]]""".stripMargin)))
        ), querki.time.epoch)

  lazy val ApplyMethod = new SystemProperty(ApplyMethodOID, QLType, Optional,
    toProps(
      setName("_apply"),
      SkillLevel(SkillLevelAdvanced),
      Summary("A QL Expression that will be run when you name this Thing."),
      Details("""_apply is an advanced function, and most users will not use it directly. But it is probably
          |the most important Property in Querki, and advanced users may want to play with it.
          |
          |One of Querki's design goals was that it should Just Work. This is reflected, more than anywhere else,
          |in the fact that you can just say:
          |[[_code(""[[My Thing]]"")]]
          |and it shows up as a pointer to *My Thing*.
          |
          |That seems obvious, but consider -- you can also say:
          |[[_code(""[[My Property]]"")]]
          |and what you get isn't a pointer to *My Property* -- instead, you get the *value* of My Property on the
          |Thing you're looking at.
          |
          |Moreover, you can say:
          |[[_code(""[[All Things]]"")]]
          |on a page, and what you get is a listing of all of the Things in this Space! So what the heck is going on
          |here?
          |
          |The secret behind the magic is the _apply method. _apply is a Property that is defined on *every* Thing.
          |(More or less -- system-defined Things use a closely-related built-in mechanism.) It defines exactly
          |"What should happen when I name this Thing?" So _apply on Properties displays the value of the Property
          |on the received Thing; _apply on All Things is this QL Expression:
          |[[_code(All Things._apply)]]
          |And _apply for Thing (the Model that everything is based on) simply produces a pointer to this thing.
          |
          |You can define _apply for your own Things as well -- indeed, the way you usually write your own serious
          |Methods is to define a Thing that just has an _apply Property, and then you can use the Method just like
          |the system-defined ones, by name.
          |
          |The QL Expression in the _apply Property will receive whatever is passed in, and should produce whatever
          |you want to pass out. It is currently completely unstructured and untyped. However, note that we will
          |probably be moving towards more structure in the future, and you should always try to be consistent:
          |as with any QL Expression, you should expect to receive a specific Type, and always produce a specific Type.""".stripMargin)))

  lazy val NotInheritedProp = new SystemProperty(NotInheritedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Not Inherited"),
      // Need to define this explicitly, to break infinite loops in lookup:
      (NotInheritedOID -> ExactlyOne(querki.values.ElemValue(false, new DelegatingType({YesNoType})))),
      AppliesToKindProp(Kind.Property),
      SkillLevel(SkillLevelAdvanced),
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
  
  lazy val NameProp = new SystemProperty(NameOID, NameType, ExactlyOne,
      toProps(
        setName("Name"),
        NotInherited,
        Summary("The name of this Thing"),
        Details("""Names should consist mainly of letters and numbers; they may also include dashes
        		|and spaces. They should usually start with a letter, and they shouldn't be
        		|excessively long.""".stripMargin)
        ))

  /**
   * The Property that points from a Property to its Type.
   */
  lazy val TypeProp = new SystemProperty(TypePropOID, LinkType, ExactlyOne,
      toProps(
        setName("Property Type"),
        LinkKindProp(Kind.Type),
        LinkAllowAppsProp(true),
        AppliesToKindProp(Kind.Property),
        NotInherited,
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
        setName("Property Collection"),
        LinkKindProp(Kind.Collection),
        LinkAllowAppsProp(true),
        AppliesToKindProp(Kind.Property),
        NotInherited,
        Summary("How much this Property holds"),
        Details("""Collection is a subtle but important concept. A Property Value isn't necessarily *one* of a
            |Type -- it can take several forms, and you have to say which one it is. As of this writing, the
            |available Collections are:
            |
            |* Exactly One -- the most common case, where the Property holds one of this Type. Note that this means
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
            |best to go for Exactly One, but you should use any of these when they make sense.""".stripMargin)
        ))

  /**
   * A flag set on a Thing to indicate that it should be used as a Model. Note that this
   * Property is not inherited: the child of a Model is not usually a Model.
   */
  lazy val IsModelProp = new SystemProperty(IsModelOID, YesNoType, ExactlyOne,
      toProps(
        setName("Is a Model"),
        NotInherited,
        // TBD: we might allow Property Models down the road, but not yet:
        AppliesToKindProp(Kind.Thing),
        SkillLevel(SkillLevelAdvanced),
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
      (InternalPropOID -> Optional(YesNoType(true))),
      Summary("If set, this Property is system-internal, and should not be visible to end users."),
      Details("""Pretty much by definition, you should never need to use this meta-Property.""".stripMargin)))

  override lazy val props = Seq(
    ApplyMethod,
    NotInheritedProp,
    UrProp,
    NameProp,
    TypeProp,
    CollectionProp,
    IsModelProp,
    AppliesToKindProp,
    InternalProp
  )
}
