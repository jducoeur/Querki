package models.system

import play.api._

import models._

import Property._
import Thing._

import querki.ecology._

import querki.values.SpaceState

/**
 * This is the master wrapper for the System Space. This is a hardcoded Space, living in
 * Shard 0. Note that all the OIDs are hardcoded, specifically so that they will be
 * stable. *NEVER* change an OID in this file!!!
 */
object SystemSpace {

  // The Types and Collections need to be defined separately, in Types.scala, to avoid initialization loop
  // errors on startup. This means we have to be careful to keep these lists in synch!
  
  // Properties:
  val props = OIDMap[Property[_,_]]()
      
  // Things:
  val things = OIDMap[ThingState]()
  
  def initialSystemState(ecology:Ecology) = {
    SpaceState(SystemIds.systemOID, querki.core.MOIDs.RootOID,
      toProps(
        // TODO: this all needs to get added *after* Core is set up!
//        setName("System")
//        (querki.basic.MOIDs.DisplayTextOID -> ExactlyOne(LargeTextType("""### Things in [[Display Name]]
//[[All Things]]
//            
//[[Tag Set Type -> 
//  _propsOfType ->
//  _sort ->
//  _section(
//    ""### Tags"", 
//    ""**____**: [[_tagsForProperty -> _sort -> _join("", "")]]
//"")]]
//            
//[[How It Works -> _if(_isDefined, ""**____**"")]]
//"""))),
//        (querki.tags.MOIDs.ShowUnknownOID -> ExactlyOne(TextType(querki.tags.defaultDisplayText)))
        ), 
      querki.identity.MOIDs.SystemUserOID, "System", querki.time.epoch, None, OIDMap[PType[_]](), props, things, OIDMap[Collection](), None, ecology)    
  }
}