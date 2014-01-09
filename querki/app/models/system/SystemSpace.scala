package models.system

import play.api._

import models._

import Property._
import Thing._

import querki.ecology.Ecology

import querki.values.SpaceState

/**
 * The Object IDs of the core System objects. These are collected here to avoid accidental
 * namespace contention -- when you add an object, you add a value to the end of this table.
 * 
 * OIDs here are *permanent*, and should never change -- since the values will wind up in the
 * database, you mustn't alter them. If you need to change behaviour, deprecate the old value
 * and add a new one.
 * 
 * Note that we have 16 bits of namespace here -- this table defines what amounts to Module 0.
 * That *should* be plenty for the long run, since most functionality should go into Modules.
 * 
 * Note that there are a few apparently holes in this run; that is because of Things that were
 * originally defined in System but got moved to Modules. We should avoid doing that -- favor
 * putting things in Modules unless they are clearly core.
 * 
 * Commented-out values, by and large, have been moved out to various Modules. At this point,
 * don't add more items here too casually.
 */
object OIDs {
  
  def sysId(local:Int) = OID(0, local)
  
  /**
   * This is an OID that is used for internal never-persisted, never-pointed-to objects.
   * Use with great caution.
   */
  val IllegalOID = sysId(-1)
  
  /**
   * The OID of the System Space itself. All these Things are contained in it.
   */
  val systemOID = sysId(0)
  
  val RootOID = sysId(1)
  val IntTypeOID = sysId(2)
  val TextTypeOID = sysId(3)
  val YesNoTypeOID = sysId(4)
//  val UrPropOID = sysId(5)
//  val NameOID = sysId(6)
  val DisplayTextOID = sysId(7)
//  val PageOID = sysId(8)
  val SystemUserOID = sysId(9)
  val NameTypeOID = sysId(10)
  val TestUserOID = sysId(11)
  val UrCollectionOID = sysId(12)
  val ExactlyOneOID = sysId(13)
  val OptionalOID = sysId(14)
  val QListOID = sysId(15)
  val LinkTypeOID = sysId(16)
//  val TypePropOID = sysId(17)
//  val CollectionPropOID = sysId(18)
  val PlaceholderTextOID = sysId(19)
  val PromptOID = sysId(20)
  val LargeTextTypeOID = sysId(21)
//  val IsModelOID = sysId(22)
//  val SimpleThingOID = sysId(23)
  val NotInheritedOID = sysId(24)
//  val StylesheetOID = sysId(25)
  val DisplayNameOID = sysId(26)
//  val CSSTextOID = sysId(27)
//  val CSSOID = sysId(28)
//  val StylesheetBaseOID = sysId(29)
//  val PhotoBaseOID = sysId(30)
  val PrototypeUserOID = sysId(31)
//  val GoogleFontOID = sysId(32)
  val LinkKindOID = sysId(33)
  val LinkAllowAppsOID = sysId(34)
  val LinkModelOID = sysId(35)
//  val AppliesToKindOID = sysId(36)
  val PlainTextOID = sysId(37)
  val InternalMethodOID = sysId(38)
  val QUnitOID = sysId(39)
//  val InternalPropOID = sysId(40)
  val ExternalLinkTypeOID = sysId(41)
//  val EditMethodOID = sysId(42)
//  val SectionMethodOID = sysId(43)
//  val InstancesMethodOID = sysId(44)
  val QLTypeOID = sysId(45)
//  val ApplyMethodOID = sysId(46)
  val InstanceEditPropsOID = sysId(47)
//  val RefsMethodOID = sysId(48)
//  val OrMethodOID = sysId(49)
//  val FirstMethodOID = sysId(50)
//  val LinkButtonOID = sysId(51)
//  val IsEmptyOID = sysId(52)
//  val IsNonEmptyOID = sysId(53)
//  val PluralizeOID = sysId(54)
//  val FilterOID = sysId(55)
//  val BulletedOID = sysId(56)
//  val NotOID = sysId(57)
//  val RestMethodOID = sysId(58)
//  val SpaceMethodOID = sysId(59)
//  val ExternalRootsOID = sysId(60)
//  val SortMethodOID = sysId(61)
//  val ChildrenMethodOID = sysId(62)
//  val IsModelMethodOID = sysId(63)
//  val IfMethodOID = sysId(64)
//  val JoinMethodOID = sysId(65)
//  val DisplayThingTreeOID = sysId(66)
//  val AllThingsOID = sysId(67)
//  val IconButtonOID = sysId(68)
//  val CreateInstanceLinkOID = sysId(69)
  val LinkToModelsOnlyOID = sysId(70)
  val TagSetOID = sysId(71)
//  val TagRefsOID = sysId(72)
  val ShowUnknownOID = sysId(73)
//  val TagsForPropertyOID = sysId(74)
//  val SelfMethodOID = sysId(75)
//  val PropsOfTypeOID = sysId(76)
//  val CodeMethodOID = sysId(77)
//  val IsDefinedOID = sysId(78)
  val QSetOID = sysId(79)
//  val CommasMethodOID = sysId(80)
//  val FormLineMethodOID = sysId(81) 
//  val EditOrElseMethodOID = sysId(82)
//  val AllPropsMethodOID = sysId(83)
//  val AllPropsThingOID = sysId(84)
//  val PropSummaryOID = sysId(85)
//  val PropDetailsOID = sysId(86)
//  val CountMethodOID = sysId(87)
//  val ReverseMethodOID = sysId(88)
//  val DescMethodOID = sysId(89)
//  val OIDMethodOID = sysId(90)
//  val KindMethodOID = sysId(91)
//  val CurrentSpaceMethodOID = sysId(92)
//  val IsMethodOID = sysId(93)
//  val EqualsMethodOID = sysId(94)
//  val ShowLinkMethodOID = sysId(95)
//  val PropLinkMethodOID = sysId(96)
  val SystemIdentityOID = sysId(97)
  val TestIdentityOID = sysId(98)
  val PrototypeIdentityOID = sysId(99)
//  val MinTextLengthOID = sysId(100)
//  val DeprecatedOID = sysId(101)
  val NewTagSetOID = sysId(102)
  val NoCreateThroughLinkOID = sysId(103)
//  val IsFunctionOID = sysId(104)
}

/**
 * This is the master wrapper for the System Space. This is a hardcoded Space, living in
 * Shard 0. Note that all the OIDs are hardcoded, specifically so that they will be
 * stable. *NEVER* change an OID in this file!!!
 */
object SystemSpace {
  
  import OIDs._

  // The Types and Collections need to be defined separately, in Types.scala, to avoid initialization loop
  // errors on startup. This means we have to be careful to keep these lists in synch!
  
  // Properties:
  val props = OIDMap[Property[_,_]](
//      UrProp, 
//      NameProp, 
//      DisplayTextProp, 
//      TypeProp, 
//      CollectionProp,
      PlaceholderTextProp, 
      PromptProp, 
//      IsModelProp, 
//      NotInheritedProp,
//      DisplayNameProp,
      LinkKindProp,
      LinkAllowAppsProp,
      LinkModelProp,
//      AppliesToKindProp,
//      InternalProp,
//      EditMethod,
//      SectionMethod,
//      InstancesMethod,
//      ApplyMethod,
      InstanceEditPropsProp,
//      RefsMethod,
//      OrMethod,
//      FirstMethod,
//      RestMethod,
//      LinkButtonMethod,
//      IsEmptyMethod,
//      IsNonEmptyMethod,
//      PluralizeMethod,
//      FilterMethod,
//      NotMethod,
//      SpaceMethod,
//      ExternalRootsMethod,
//      SortMethod,
//      ChildrenMethod,
//      IsModelMethod,
//      IfMethod,
//      JoinMethod,
//      IconButtonMethod,
//      CreateInstanceLinkMethod,
      LinkToModelsOnlyProp,
//      TagRefsMethod,
      ShowUnknownProp,
//      TagsForPropertyMethod,
//      SelfMethod,
//      PropsOfTypeMethod,
//      CodeMethod,
//      IsDefinedMethod,
//      FormLineMethod,
//      EditOrElseMethod,
//      AllPropsMethod,
//      PropSummary,
//      PropDetails,
//      CountMethod,
//      ReverseMethod,
//      DescMethod,
//      OIDMethod,
//      KindMethod,
//      CurrentSpaceMethod,
//      IsMethod,
//      EqualsMethod,
//      ShowLinkMethod,
//      PropLinkMethod,
//      DeprecatedProp,
      NoCreateThroughLinkProp
//      IsFunctionProp
      )
      
  // Things:
  val things = OIDMap[ThingState](UrThing)
  
  def initialSystemState(ecology:Ecology) = {
    SpaceState(systemOID, RootOID,
      toProps(
        setName("System"),
        (querki.basic.MOIDs.DisplayTextOID -> ExactlyOne(LargeTextType("""### Things in [[Display Name]]
[[All Things]]
            
[[Tag Set Type -> 
  _propsOfType ->
  _sort ->
  _section(
    ""### Tags"", 
    ""**____**: [[_tagsForProperty -> _sort -> _join("", "")]]
"")]]
            
[[How It Works -> _if(_isDefined, ""**____**"")]]
"""))),
        ShowUnknownProp(TagThing.defaultDisplayText)), 
      SystemUserOID, "System", querki.time.epoch, None, SystemTypes.all, props, things, SystemCollections.all, None, ecology)    
  }
}