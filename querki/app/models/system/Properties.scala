package models.system

import play.api.Logger

import models._

import Property._
import Thing._

import OIDs._
import SystemSpace._

import querki.values._

class SystemProperty[VT, -RT](pid:OID, t:PType[VT] with PTypeBuilder[VT, RT], c:Collection, p:PropFetcher) 
  extends Property[VT, RT](pid, systemOID, UrPropOID, t, c, p, modules.time.TimeModule.epoch)

  /**
   * The root Property, from which all others derive.
   */
  object UrProp extends Property(UrPropOID, systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName("Property"),
        (PropSummaryOID -> Optional(TextType("The root Property, from which all others derive."))),
        (DisplayTextOID -> Optional(LargeTextType("""[[Property Summary -> ""**____** -- ""]]
            |[[_if(Property Type -> _is(Internal Method Type), 
            |  ""**METHOD**"",
            |  ""Collection: [[Property Collection]] Type: [[Property Type]]"")]]
            |
            |
            |[[Property Details]]""".stripMargin)))
        ), modules.time.TimeModule.epoch)
  
  object NameProp extends SystemProperty(NameOID, NameType, ExactlyOne,
      toProps(
        setName("Name"),
        placeholderText("Name..."),
        NotInheritedProp(true),
        PropSummary("The name of this Thing"),
        PropDetails("""Names should consist mainly of letters and numbers; they may also include dashes
        		|and spaces. They should usually start with a letter, and they shouldn't be
        		|excessively long.""".stripMargin)
        ))
  
  // TODO: the name DisplayTextProp still need to be renamed to DefaultViewProp:
  object DisplayTextProp extends SystemProperty(DisplayTextOID, LargeTextType, Optional,
      toProps(
        setName("Default View"),
        (PropSummaryOID -> ExactlyOne(TextType("How this Thing will be displayed"))),
        (PropDetailsOID -> ExactlyOne(LargeTextType("""Default View is one of the most important Properties in Querki,
        		|and nearly every Thing has one. The Default View describes how this Thing will usually show up when you
        		|look at it as a web page. It can say almost anything you like, but usually consists of a mix of
        		|text and QL expressions. (Where a "QL Expression" is anything inside double-square-brackets.)""".stripMargin)))
        ))

  /**
   * The Property that points from a Property to its Type.
   */
  object TypeProp extends SystemProperty(TypePropOID, LinkType, ExactlyOne,
      toProps(
        setName("Property Type"),
        LinkKindProp(Kind.Type),
        LinkAllowAppsProp(true),
        AppliesToKindProp(Kind.Property),
        NotInheritedProp(true),
        PropSummary("The Type that this Property can hold"),
        PropDetails("""A Type is something like "Text" or "Number" or "Tag". Every Property must be designed for
        		|exactly one Type, and only values of that Type can be placed in it.
        		|
        		|Most Properties are of type Text or Large Text (which is the same thing, but can have multiple lines);
        		|if you don't know what to use, that's usually your best bet.""".stripMargin)
        ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  object CollectionProp extends SystemProperty(CollectionPropOID, LinkType, ExactlyOne,
      toProps(
        setName("Property Collection"),
        LinkKindProp(Kind.Collection),
        LinkAllowAppsProp(true),
        AppliesToKindProp(Kind.Property),
        NotInheritedProp(true),
        PropSummary("How much this Property holds"),
        PropDetails("""Collection is a subtle but important concept. A Property Value isn't necessarily *one* of a
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
    
  object PlaceholderTextProp extends SystemProperty(PlaceholderTextOID, PlainTextType, Optional,
      toProps(
        setName("Placeholder Text"),
        AppliesToKindProp(Kind.Property),
        PropSummary("Placeholder text for input boxes"),
        PropDetails("""In Text Properties, it is often helpful to have a prompt that displays inside the input
            |field until the user begins to type something there. If the Property has a Placeholder Text, that
            |will be displayed in grey when the input is first shown.""".stripMargin)
        ))
  
  object PromptProp extends SystemProperty(PromptOID, PlainTextType, Optional,
      toProps(
        setName("Prompt"),
        AppliesToKindProp(Kind.Property),
        PropSummary("Prompt to use in the Editor"),
        PropDetails("""In the Editor, Properties are usually displayed with their Name. If you want to show something
            |other than the Name, set the Prompt Property to say what you would like to show instead.""".stripMargin)
        ))

  /**
   * A flag set on a Thing to indicate that it should be used as a Model. Note that this
   * Property is not inherited: the child of a Model is not usually a Model.
   */
  object IsModelProp extends SystemProperty(IsModelOID, YesNoType, ExactlyOne,
      toProps(
        setName("Is a Model"),
        NotInheritedProp(true),
        // TBD: we might allow Property Models down the road, but not yet:
        AppliesToKindProp(Kind.Thing),
        SkillLevel(SkillLevel.Advanced),
        PropSummary("Is this Thing a Model?"),
        PropDetails("""All Things can be used as Models if you would like -- in Querki, unlike most programming
            |languages, the difference between a Model and an Instance is pretty small. But this flag indicates
            |that this Thing should *normally* be considered a Model. If you set it, then this Model will be
            |offered as a possibility when you go to create a Thing, it will be displayed a bit differently in
            |the Space's list of its Things, and in general will be handled a little differently.""".stripMargin)
        ))

/**
 * Meta-property: if this Property has NotInherited set, then its values are not inherited from its parent.
 */
object NotInheritedProp extends SystemProperty(NotInheritedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Not Inherited"),
      // Need to define this explicitly, to break infinite loops in lookup:
      (NotInheritedOID -> ExactlyOne(ElemValue(false, new DelegatingType({YesNoType})))),
      AppliesToKindProp(Kind.Property),
      PropSummary("Should this Property be inherited from ancestors?"),
      PropDetails("""All Things in Querki are part of a big inheritance tree. Instances inherit from their Models,
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

/**
 * If set, this is the display name of the specified object. Whereas the primary NameProp
 * has a number of restrictions, the DisplayNameProp does not. It is used to list a Thing
 * by preference when it is set.
 */
object DisplayNameProp extends SystemProperty(DisplayNameOID, PlainTextType, Optional,
    toProps(
      setName("Display Name"),
      NotInheritedProp(true),
      modules.Modules.Types.MinTextLengthProp(1),
      PropSummary("How to show this Thing's Name"),
      PropDetails("""Most Things in Querki have a Name. (It isn't strictly required, but strongly encouraged most
          |of the time.) In general, when we list a Thing, we show its Name. However, if you want to display
          |something *other* than its Name instead, set its Display Name Property to show in its place.
          |
          |Display Name is mainly useful when the name you would like to use includes characters that aren't
          |legal in Names, such as quotes, apostrophes, commas or other punctuation characters.
          |
          |Note that the relationship of Name and Display Name is still in some flux, and things may shift a
          |bit over time. We are thinking of putting Display Name more front-and-center, and making Name derive
          |from that instead.""".stripMargin)
      ))

/**
 * Meta-property, set on Properties of LinkType, to filter what to Link to.
 */
object LinkKindProp extends SystemProperty(LinkKindOID, IntType, QList,
    toProps(
      setName("Link Kind"),
      PropSummary("The Kind that this Property can Link to"),
      PropDetails("""When you create a Link Property, if you do *not* set the *Link Model* Property on it,
          |you may want to at least specify which *Kind* of Thing this can Link to. There are five Kinds of
          |Things in Querki:
          |
          |* Ordinary Thing
          |* Property
          |* Space
          |* Type
          |* Collection
          |
          |99% of the time, you will want to link to ordinary Things. (And most of those times, you should set
          |a particular Link Model.) Occasionally, for very complex systems, you may want to Link to Property
          |or Space instead. You are not likely to ever link to Type or Collection, but it is possible to do so.
          |
          |This is an extremely advanced property, and not intended for casual use.""".stripMargin),
      AppliesToKindProp(Kind.Property)
      ))

object LinkAllowAppsProp extends SystemProperty(LinkAllowAppsOID, YesNoType, Optional,
    toProps(
      setName("Allow Links to Apps"),
      PropSummary("Should this Property allow Links to Things in Apps?"),
      PropDetails("""Normally, links are only to other Things in this Space. But if this flag is set, this
          |says that this Property should allow linking to Things in Apps of this Space.
          |
          |This is an advanced property, and not intended for casual use.""".stripMargin),
      AppliesToKindProp(Kind.Property)
      ))

object LinkModelProp extends SystemProperty(LinkModelOID, LinkType, Optional,
    toProps(
      setName("Link Model"),
      PropSummary("Which Things can this Property link to?"),
      PropDetails("""By default, Link Properties allow you to link to *anything*. This usually isn't what
          |you want, though -- most often, you're looking for Instances of a specific Model. For example,
          |if you specify the Stylesheet Property, you only want to give Stylesheets as options to Link to:
          |it would be meaningless to have the Stylesheet point to something like a recipe or a to-do list.
          |
          |So this is a meta-Property: when you create a Property that is a Link, you can add this to say
          |exactly what it can link *to*. It is strongly recommended that you set this on all Link Properties
          |you create -- it makes them easier to use, and tends to prevent confusing errors.
          |
          |Note that this is only enforced loosely, and you can't absolutely count upon this restriction
          |always being true. But used properly, it will steer folks in the right direction.""".stripMargin),
      AppliesToKindProp(Kind.Property),
      LinkToModelsOnlyProp(true)
      ))

// TODO: As it says, replace this with a more general Link Filter property. That will need bigger
// refactorings, though: I started to build that, only to discover that SpaceState.linkCandidates
// doesn't have all the request-context information needed to resolve a QL Expression.
object LinkToModelsOnlyProp extends SystemProperty(LinkToModelsOnlyOID, YesNoType, ExactlyOne,
    toProps(
      setName("Link to Models Only"),
      PropSummary("Only allow this Property to Link to Models"),
      PropDetails("""If set to true, this Link Property will only show Models as options to link to in the editor.
          |
          |This is an advanced property, and something of a hack -- don't get too comfortable with it. In the
          |medium term, it should get replaced by a more general LinkFilter property that lets you specify which
          |Things to link to.""".stripMargin)))

object AppliesToKindProp extends SystemProperty(AppliesToKindOID, IntType, QList,
    toProps(
      setName("Applies To"),
      (AppliesToKindOID -> QList(ElemValue(Kind.Property, new DelegatingType(IntType)))),
      (PropSummaryOID -> Optional(TextType("Which Kinds of Things can this Property be used on?"))),
      (PropDetailsOID -> Optional(LargeTextType("""By default, a Property can be used on anything -- even when
          |that is nonsensical. The result is that, when creating a new Thing, you get a messy list of lots of
          |Properties, many of which are irrelevant.
          |
          |So to keep that from happening, use this on your Properties. In most cases, a Property is really intended
          |to only apply to Things *or* Properties, not both. So using this can keep you from having a long and
          |confusing Property List.
          |
          |This is an advanced Property, mostly because most user-defined Properties are intended to be used on
          |Things, and you usually want to see all of your user-defined Properties as options. But serious
          |programmers working in Querki may want to set this, especially if they are creating meta-Properties.""".stripMargin)))
      ))

object InternalProp extends SystemProperty(InternalPropOID, YesNoType, Optional,
    toProps(
      setName("Internal Property"),
      AppliesToKindProp(Kind.Property),
      NotInheritedProp(true),
      (InternalPropOID -> Optional(YesNoType(true))),
      PropSummary("If set, this Property is system-internal, and should not be visible to end users."),
      PropDetails("""Pretty much by definition, you should never need to use this meta-Property.""".stripMargin)))

// TODO: this should really only allow the properties that are defined on this Model:
object InstanceEditPropsProp extends SystemProperty(InstanceEditPropsOID, LinkType, QList,
    toProps(
      setName("Properties to edit in Instances"),
      LinkAllowAppsProp(true),
      LinkKindProp(Kind.Property),
      PropSummary("Which Properties should be edited in Instances of this Model?"),
      PropDetails("""It is very common to define a bunch of Properties on a Model that you really don't
          |ever intend to change on the Instances. (In particular, you very often will define the Display
          |Text on the Model, not on the Instances.) This results in your Instance Editor being cluttered
          |with lots of Properties that you never, ever use.
          |
          |So this Property is a quick-and-easy way to lay out your Instance Editor. It is a List of
          |Properties that you can define however you like. When you create or edit an Instance of this
          |Model, it will display exactly those Properties, in that order, which usually makes it
          |easier for you to write your Instances.""".stripMargin)))

object ShowUnknownProp extends SystemProperty(ShowUnknownOID, LargeTextType, ExactlyOne,
    toProps(
      setName("Undefined Tag View"),
      AppliesToKindProp(Kind.Space),
      PropSummary("What should be displayed when you click on a Tag that isn't a Thing?"),
      PropDetails("""In Querki, it is entirely legal to refer to the name of something you haven't written yet --
          |for instance, Tags are often names with no definition. So the question becomes, what should be
          |displayed when you click on one of these tags, since it doesn't point to a real Thing?
          |
          |The Undefined Tag View Property defines that. It is a Large Text that is defined on the Space; when
          |you try to look at an unknown name, it will show this text. You can put QL expressions in here; they
          |will receive the Name that you are trying to look at.
          |
          |You can also put an Undefined Tag View on a Model, which basically means that all Tags of this Model
          |will use that View. (Technically, this means all Tags that are used in a Tag Set whose Link Model
          |points to this Model.)
          |
          |There is a simple default value that is defined on every Space by default. But you should feel free
          |to override that to do something more interesting, especially if you are doing interesting things
          |with Tags in your Space.""".stripMargin)))

object PropSummary extends SystemProperty(PropSummaryOID, TextType, Optional,
    toProps(
      setName("Property Summary"),
      (NotInheritedOID -> ExactlyOne(YesNoType(true))),
      AppliesToKindProp(Kind.Property),
      (PropSummaryOID -> Optional(TextType("This is an optional one-line description of a Property."))),
      (PropDetailsOID -> Optional(LargeTextType("""When you define a Property, you may add this Summary as
          |part of that definition. It will be displayed in mouseover hovering and things like that, to help
          |you and others remember what this Property is. It's optional, but recommended that you define
          |this for all of your Properties.
          |
          |There is no required format or content for this Summary -- it should simply be a reminder of what
          |this Property is about.""".stripMargin)))))

object PropDetails extends SystemProperty(PropDetailsOID, LargeTextType, Optional,
    toProps(
      setName("Property Details"),
      (NotInheritedOID -> ExactlyOne(YesNoType(true))),
      AppliesToKindProp(Kind.Property),
      (PropSummaryOID -> Optional(TextType("This is an optional detailed description of a Property."))),
      (PropDetailsOID -> Optional(LargeTextType("""When you define a Property, you may add whatever description
          |or documentation you see fit in the Details. This is the place to say what this Property is for, what
          |it means, how it should be used, what is legal in it, and so on.
          |
    	  |It is entirely optional -- put something in here if it makes sense. In general, the more complex the
          |Space, and the more people who will be using it, the wiser it becomes to give Details for all of your
          |Properties. If this Space is simple and just for you, it usually isn't necessary.""".stripMargin)))))

object DeprecatedProp extends SystemProperty(DeprecatedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Deprecated"),
      NotInheritedProp(true),
      SkillLevel(SkillLevel.Advanced),
      PropSummary("True iff this Thing is Deprecated."),
      PropDetails("""This is a marker flag that you can put on a Thing to say that it is on its way out, and shouldn't
          |be used any more.
          |
          |The exact meaning of Deprecated depends on the situation, but Querki will tend to hide Things marked as
          |Deprecated. If you see somewhere that a Deprecated Thing is visible and shouldn't be, please log a bug
          |report about it.""".stripMargin)))

object NoCreateThroughLinkProp extends SystemProperty(NoCreateThroughLinkOID, YesNoType, ExactlyOne,
    toProps(
      setName("No Create Through Link Model"),
      NotInheritedProp(true),
      PropSummary("Set this to prevent new instances from being created accidentally."),
      PropDetails("""When you create a Link Property in the Editor, you can set the "Link Model" -- the sort of Thing
          |that this Property points to. The Editor then lets you choose from all of the existing Instances of that
          |Model, and also lets you create a new one.
          |
          |Sometimes, though, you don't want to create any new ones from the Editor. In particular, if you've already
          |created all of the Instances of this Model that you ever expect to want, then it is simply annoying to have
          |that option. In that case, put this Property on your Model, and set it to True -- it will make that option
          |in the Editor go away.""".stripMargin)))
