package models.system

import play.api._

import models._

import Property._
import Thing._

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
  val UrPropOID = sysId(5)
  val NameOID = sysId(6)
  val DisplayTextOID = sysId(7)
  val PageOID = sysId(8)
  val SystemUserOID = sysId(9)
  val NameTypeOID = sysId(10)
  val TestUserOID = sysId(11)
  val UrCollectionOID = sysId(12)
  val ExactlyOneOID = sysId(13)
  val OptionalOID = sysId(14)
  val QListOID = sysId(15)
  val LinkTypeOID = sysId(16)
  val TypePropOID = sysId(17)
  val CollectionPropOID = sysId(18)
  val PlaceholderTextOID = sysId(19)
  val PromptOID = sysId(20)
  val LargeTextTypeOID = sysId(21)
  val IsModelOID = sysId(22)
  val SimpleThingOID = sysId(23)
  val NotInheritedOID = sysId(24)
//  val StylesheetOID = sysId(25)
  val DisplayNameOID = sysId(26)
//  val CSSTextOID = sysId(27)
//  val CSSOID = sysId(28)
//  val StylesheetBaseOID = sysId(29)
  val PhotoBaseOID = sysId(30)
  val PrototypeUserOID = sysId(31)
//  val GoogleFontOID = sysId(32)
  val LinkKindOID = sysId(33)
  val LinkAllowAppsOID = sysId(34)
  val LinkModelOID = sysId(35)
  val AppliesToKindOID = sysId(36)
  val PlainTextOID = sysId(37)
  val InternalMethodOID = sysId(38)
  val QUnitOID = sysId(39)
  val InternalPropOID = sysId(40)
  val ExternalLinkTypeOID = sysId(41)
  val EditMethodOID = sysId(42)
}

/**
 * This is the master wrapper for the System Space. This is a hardcoded Space, living in
 * Shard 0. Note that all the OIDs are hardcoded, specifically so that they will be
 * stable. *NEVER* change an OID in this file!!!
 */
object SystemSpace {
  
  import OIDs._
  import Space.oidMap

  // The Types and Collections need to be defined separately, in Types.scala, to avoid initialization loop
  // errors on startup. This means we have to be careful to keep these lists in synch!
  
  // Properties:
  val props = oidMap[Property[_,_]](
      UrProp, 
      NameProp, 
      DisplayTextProp, 
      TypeProp, 
      CollectionProp,
      PlaceholderTextProp, 
      PromptProp, 
      IsModelProp, 
      NotInheritedProp,
      DisplayNameProp,
      LinkKindProp,
      LinkAllowAppsProp,
      LinkModelProp,
      AppliesToKindProp,
      EditMethod)
      
  // Things:
  val things = oidMap[ThingState](UrThing, Page, SimpleThing, PhotoBase)
  
  def init = {
    _state = Some(modules.Modules.initAllModules(initialSystemState))
  }
  
  def term = {
    modules.Modules.termAllModules
  }
  
  private def initialSystemState = {
    SpaceState(systemOID, RootOID,
      toProps(
        setName("System"),
        DisplayTextProp("""
This is the fundamental System Space. Everything else derives from it.
""")
        ), SystemUserOID, "System", None, SystemTypes.all, props, things, SystemCollections.all)    
  }
  
  // Note the intentional implication here: trying to access State before init has been
  // called will throw an exception:
  private var _state:Option[SpaceState] = None
  lazy val State = _state.get
}