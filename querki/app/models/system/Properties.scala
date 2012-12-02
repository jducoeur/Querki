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
  
  class NameProp(pid:OID) extends SystemProperty(pid, NameType, ExactlyOne,
      toProps(
        setName("Name"),
        prompt("Name of the new Thing"),
        placeholderText("Name")
        ))
  
  class DisplayTextProp(pid:OID) extends SystemProperty(pid, LargeTextType, Optional,
      toProps(
        setName("Display-Text"),
        prompt("Display text"),
        placeholderText("How this Thing shows up")
        ))

  /**
   * The Property that points from a Property to its Type.
   */
  object TypeProp extends Property(OID(0, 17), systemOID, UrPropOID, LinkType, ExactlyOne,
      toProps(
        setName("__Type")
        ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  object CollectionProp extends Property(OID(0, 18), systemOID, UrPropOID, LinkType, ExactlyOne,
      toProps(
        setName("__Collection")
        ))
    
  object PlaceholderTextProp extends Property(PlaceholderTextOID, systemOID, UrPropOID, TextType, Optional,
      toProps(
        setName("Placeholder Text")
        ))
  
  object PromptProp extends Property(PromptOID, systemOID, UrPropOID, TextType, Optional,
      toProps(
        setName("Prompt")
        ))

  /**
   * A flag set on a Thing to indicate that it should be used as a Model. Note that this
   * Property is not inherited: the child of a Model is not usually a Model.
   */
  object IsModelProp extends Property(IsModelOID, systemOID, UrPropOID, YesNoType, ExactlyOne,
      toProps(
        setName("Is a Model"),
        NotInheritedProp(true)
        ))

  /**
   * Meta-property: if this Property has NotInherited set, then its values are not inherited from its parent.
   */
  object NotInheritedProp extends Property(NotInheritedOID, systemOID, UrPropOID, YesNoType, ExactlyOne,
      toProps(
        setName("Not Inherited"),
        // Need to define this explicitly, to break infinite loops in lookup:
        (NotInheritedOID -> PropValue(OneColl(ElemValue(false))))
        ))
    