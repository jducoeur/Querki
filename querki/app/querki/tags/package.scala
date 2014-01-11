package querki

import querki.ecology._

import models.{Property, PType, Thing}
import models.system.{PlainText, QLText}

import querki.values.SpaceState

package object tags {
  val defaultDisplayText = """Referenced from:
[[_tagRefs -> _bulleted]]"""  

  object MOIDs extends EcotIds(22) {
    val TagRefsOID = sysId(72)
    val ShowUnknownOID = sysId(73)
    val TagsForPropertyOID = sysId(74)
    val NewTagSetOID = sysId(102)
  }

  trait Tags extends EcologyInterface {
    def NewTagSetType:PType[PlainText]
    
    def ShowUnknownProp:Property[QLText,String]
    
    def getTag(name:String, state:SpaceState):Thing
    def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String]
    def preferredModelForTag(implicit state:SpaceState, nameIn:String):Thing
  }
}