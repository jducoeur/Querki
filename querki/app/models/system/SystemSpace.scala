package models.system

import play.api._

import models._

import Property._
import Thing._

import OID.thing2OID

object OIDs {
  /**
   * The OID of the System Space itself. All these Things are contained in it.
   */
  val systemOID = OID(0, 0)
  
  val RootOID = OID(0, 1)
  val IntTypeOID = OID(0, 2)
  val TextTypeOID = OID(0, 3)
  val YesNoTypeOID = OID(0, 4)
  val UrPropOID = OID(0, 5)
  val NameOID = OID(0, 6)
  val DisplayTextOID = OID(0, 7)
  val SystemUserOID = OID(0, 9)
  val NameTypeOID = OID(0, 10)
  val TestUserOID = OID(0, 11)
  val UrCollectionOID = OID(0, 12)
  val ExactlyOneOID = OID(0, 13)
  val OptionalOID = OID(0, 14)
  val QListOID = OID(0, 15)
  val LinkTypeOID = OID(0, 16)
  val PlaceholderTextOID = OID(0, 19)
  val PromptOID = OID(0, 20)
  val LargeTextTypeOID = OID(0, 21)
  val IsModelOID = OID(0, 22)
  val NotInheritedOID = OID(0, 24)
}

/**
 * This is the master wrapper for the System Space. This is a hardcoded Space, living in
 * Shard 0. Note that all the OIDs are hardcoded, specifically so that they will be
 * stable. *NEVER* change an OID in this file!!!
 */
object SystemSpace {
  
  import OIDs._

  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }

  // The Types and Collections need to be defined separately, in Types.scala, to avoid initialization loop
  // errors on startup. This means we have to be careful to keep these lists in synch!
  val types = oidMap[PType[_]](IntType, TextType, YesNoType, NameType, LinkType, LargeTextType)
  val colls = oidMap[Collection[_]](UrCollection, ExactlyOne, Optional, QList)
  
  // Properties:
  val NameProp = new NameProp(NameOID)
  val DisplayTextProp = new DisplayTextProp(DisplayTextOID)
  val props = oidMap[Property[_,_,_]](
      new UrProp(UrPropOID), 
      NameProp, 
      DisplayTextProp, 
      TypeProp, 
      CollectionProp,
      PlaceholderTextProp, PromptProp, IsModelProp, NotInheritedProp)
  val things = oidMap[ThingState](UrThing, Page, SimpleThing)
  
  object State extends SpaceState(systemOID, RootOID,
      toProps(
        setName("System"),
        DisplayTextProp("""
This is the fundamental System Space. Everything else derives from it.
""")
        ), SystemUserOID, "System", None, types, props, things, colls)
}