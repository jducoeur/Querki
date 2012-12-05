package models.system

import models._

import Property._
import Thing._

import OIDs._
import SystemSpace._

class SystemProperty[VT, -RT, CT](pid:OID, t:PType[VT] with PTypeBuilder[VT, RT], c:Collection[CT], p:PropFetcher) extends
  Property(pid, systemOID, UrPropOID, t, c, p)

  /**
   * The root Property, from which all others derive.
   */
  class UrProp(pid:OID) extends Property(pid, systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName("Property"),
        (PromptOID -> PropValue(None)),
        (PlaceholderTextOID -> PropValue(None)),
        (NotInheritedOID -> PropValue(OneColl(ElemValue(false))))
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
        setName("__Type")
        ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  object CollectionProp extends SystemProperty(CollectionPropOID, LinkType, ExactlyOne,
      toProps(
        setName("__Collection")
        ))
    
  object PlaceholderTextProp extends SystemProperty(PlaceholderTextOID, TextType, Optional,
      toProps(
        setName("Placeholder Text")
        ))
  
  object PromptProp extends SystemProperty(PromptOID, TextType, Optional,
      toProps(
        setName("Prompt")
        ))

  /**
   * A flag set on a Thing to indicate that it should be used as a Model. Note that this
   * Property is not inherited: the child of a Model is not usually a Model.
   */
  object IsModelProp extends SystemProperty(IsModelOID, YesNoType, ExactlyOne,
      toProps(
        setName("Is a Model"),
        NotInheritedProp(true)
        ))

/**
 * Meta-property: if this Property has NotInherited set, then its values are not inherited from its parent.
 */
object NotInheritedProp extends SystemProperty(NotInheritedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Not Inherited"),
      // Need to define this explicitly, to break infinite loops in lookup:
      (NotInheritedOID -> PropValue(OneColl(ElemValue(false))))
      ))

/**
 * Points to the optional CSS file for this Thing. If placed on a Space, applies Space-wide.
 * 
 * TBD: this isn't quite so "system-ish". We might start defining properties nearer to their
 * relevant functionality.
 */
object StylesheetProp extends SystemProperty(StylesheetOID, LinkType, Optional,
    toProps(
      setName("Stylesheet")
      ))

/**
 * If set, this is the display name of the specified object. Whereas the primary NameProp
 * has a number of restrictions, the DisplayNameProp does not. It is used to list a Thing
 * by preference when it is set.
 */
object DisplayNameProp extends SystemProperty(DisplayNameOID, TextType, Optional,
    toProps(
      setName("Display Name")
      ))