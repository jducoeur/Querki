package models.system

import models._

import Property._
import Thing._

import OIDs._
import SystemSpace._

class SystemProperty[VT, -RT](pid:OID, t:PType[VT] with PTypeBuilder[VT, RT], c:Collection, p:PropFetcher) 
  extends Property[VT, RT](pid, systemOID, UrPropOID, t, c, p)

  /**
   * The root Property, from which all others derive.
   */
  object UrProp extends Property(UrPropOID, systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName("Property"),
        (PromptOID -> Optional.None),
        (PlaceholderTextOID -> Optional.None),
        (NotInheritedOID -> Optional(ElemValue(false)))
        ))
  
  object NameProp extends SystemProperty(NameOID, NameType, ExactlyOne,
      toProps(
        setName("Name"),
        prompt("Name of the new Thing"),
        placeholderText("Name")
        ))
  
  object DisplayTextProp extends SystemProperty(DisplayTextOID, LargeTextType, Optional,
      toProps(
        setName("Display-Text"),
        prompt("Display text"),
        placeholderText("How this Thing shows up")
        ))

  /**
   * The Property that points from a Property to its Type.
   */
  object TypeProp extends SystemProperty(TypePropOID, LinkType, ExactlyOne,
      toProps(
        setName("__Type"),
        prompt("What Type does this Property hold"),
        LinkKindProp(Kind.Type),
        LinkAllowAppsProp(true),
        AppliesToKindProp(Kind.Property)
        ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  object CollectionProp extends SystemProperty(CollectionPropOID, LinkType, ExactlyOne,
      toProps(
        setName("__Collection"),
        prompt("How many are contained in this Property"),
        LinkKindProp(Kind.Collection),
        LinkAllowAppsProp(true),
        AppliesToKindProp(Kind.Property)
        ))
    
  object PlaceholderTextProp extends SystemProperty(PlaceholderTextOID, PlainTextType, Optional,
      toProps(
        setName("Placeholder Text"),
        AppliesToKindProp(Kind.Property)
        ))
  
  object PromptProp extends SystemProperty(PromptOID, PlainTextType, Optional,
      toProps(
        setName("Prompt"),
        AppliesToKindProp(Kind.Property)
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
        AppliesToKindProp(Kind.Thing)
        ))

/**
 * Meta-property: if this Property has NotInherited set, then its values are not inherited from its parent.
 */
object NotInheritedProp extends SystemProperty(NotInheritedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Not Inherited"),
      // Need to define this explicitly, to break infinite loops in lookup:
      (NotInheritedOID -> ExactlyOne(ElemValue(false))),
      AppliesToKindProp(Kind.Property)
      ))

/**
 * If set, this is the display name of the specified object. Whereas the primary NameProp
 * has a number of restrictions, the DisplayNameProp does not. It is used to list a Thing
 * by preference when it is set.
 */
object DisplayNameProp extends SystemProperty(DisplayNameOID, PlainTextType, Optional,
    toProps(
      setName("Display Name")
      ))

/**
 * Meta-property, set on Properties of LinkType, to filter what to Link to.
 */
object LinkKindProp extends SystemProperty(LinkKindOID, IntType, QList,
    toProps(
      setName("Link Kind"),
      prompt("Kind that this Property can Link to"),
      DisplayTextProp("""
By and large, Link Properties should always point to a particular kind -- it should point to
Things, Properties, Types, or Collections. This says which Kind is allowed.
          
This is an extremely advanced property, and not intended for casual use.
"""),
      AppliesToKindProp(Kind.Property)
      ))

object LinkAllowAppsProp extends SystemProperty(LinkAllowAppsOID, YesNoType, Optional,
    toProps(
      setName("Allow Links to Apps"),
      DisplayTextProp("""
Links, by default, are only to other Things in the same Space. If set, this says that this
Property should allow linking to Things in Apps.
          
This is an extremely advanced property, and not intended for casual use.
"""),
      AppliesToKindProp(Kind.Property)
      ))

object LinkModelProp extends SystemProperty(LinkModelOID, LinkType, Optional,
    toProps(
      setName("Link Model"),
      DisplayTextProp("""
By default, Link Properties allow you to link to *anything*. This usually isn't what you want --
most often, you're looking for Things under a specific Model. For example, if you specify the
Stylesheet Property, you only want to give Stylesheets as options to Link to.
          
So this is a meta-Property: when you create a Property that is a Link, you can add this to
say exactly what it can link *to*.
          
Note that this is only enforced loosely, and you can't absolutely count upon this restriction
always being true. But used properly, it will steer folks in the right direction.
"""),
      AppliesToKindProp(Kind.Property)
      ))

object AppliesToKindProp extends SystemProperty(AppliesToKindOID, IntType, QList,
    toProps(
      setName("Applies To"),
      (AppliesToKindOID -> QList(ElemValue(Kind.Property))),
      DisplayTextProp("""
By default, a Property can be used on anything -- even when that is nonsensical. The
result is that, when creating a new Thing, you get a messy list of lots of Properties,
many of which are irrelevant.
          
So to keep that from happening, use this on your Properties. In most cases, a Property
is really intended to only apply to Things *or* Properties, not both. So using this
will keep you from having a long and confusing Property List.
""")
      ))