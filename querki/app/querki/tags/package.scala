package querki

import querki.ecology._

import models.{Property, PType, PTypeBuilder, Thing}

import querki.values.SpaceState

import querki.core.QLText
import querki.basic.PlainText

package object tags {
  val defaultDisplayText = """Referenced from:
[[_tagRefs -> _bulleted]]"""  

  object MOIDs extends EcotIds(22) {
    val TagSetOID = sysId(71)
    val TagRefsOID = sysId(72)
    val ShowUnknownOID = sysId(73)
    val TagsForPropertyOID = sysId(74)
    val NewTagSetOID = sysId(102)
  }

  trait Tags extends EcologyInterface {
    def TagSetType:PType[String] with PTypeBuilder[String,String]
    def NewTagSetType:PType[PlainText] with PTypeBuilder[PlainText, String]
    
    def ShowUnknownProp:Property[QLText,String]
    
    def getTag(name:String, state:SpaceState):Thing
    def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String]
    def preferredModelForTag(implicit state:SpaceState, nameIn:String):Thing
  }
}