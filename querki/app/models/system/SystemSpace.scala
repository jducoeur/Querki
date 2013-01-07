package models.system

import play.api._

import models._

import Property._
import Thing._

object OIDs {
  
  def sysId(local:Int) = OID(0, local)
  
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
  val StylesheetOID = sysId(25)
  val DisplayNameOID = sysId(26)
  val CSSTextOID = sysId(27)
  val CSSOID = sysId(28)
  val StylesheetBaseOID = sysId(29)
  val PhotoBaseOID = sysId(30)
  val PrototypeUserOID = sysId(31)
  val GoogleFontOID = sysId(32)
  val LinkKindOID = sysId(33)
  val LinkAllowAppsOID = sysId(34)
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
  val props = oidMap[Property[_,_,_]](
      new UrProp(UrPropOID), 
      NameProp, 
      DisplayTextProp, 
      TypeProp, 
      CollectionProp,
      PlaceholderTextProp, 
      PromptProp, 
      IsModelProp, 
      NotInheritedProp,
      StylesheetProp,
      DisplayNameProp,
      CSSProp,
      GoogleFontProp,
      LinkKindProp,
      LinkAllowAppsProp)
      
  // Things:
  val things = oidMap[ThingState](UrThing, Page, SimpleThing, StylesheetBase, PhotoBase)
  
  object State extends SpaceState(systemOID, RootOID,
      toProps(
        setName("System"),
        DisplayTextProp("""
This is the fundamental System Space. Everything else derives from it.
""")
        ), SystemUserOID, "System", None, SystemTypes.all, props, things, SystemCollections.all)
}