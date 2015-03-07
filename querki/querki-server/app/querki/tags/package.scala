package querki

import querki.ecology._

import models.{Property, PType, PTypeBuilder, Thing}

import querki.globals._

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
    
    val ResolveTagsOID = moid(1)
  }
  
  // HACK: this is a marker trait so that we can treat Tags a bit differently in thing.scala.html.
  // TODO: find a more elegant solution to this.
  trait IsTag

  trait Tags extends EcologyInterface {
    def TagSetType:PType[String] with PTypeBuilder[String,String]
    def NewTagSetType:PType[PlainText] with PTypeBuilder[PlainText, String]
    
    def ShowUnknownProp:Property[QLText,String]
    
    def getTag(name:String, state:SpaceState):Thing
    def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String]
    def preferredModelForTag(implicit state:SpaceState, nameIn:String):Thing
    /**
     * Returns true iff this Property is "taggable" -- that is, Manifest can treat it like a tag.
     */
    def isTaggableProperty(prop:AnyProp)(implicit state:SpaceState):Boolean
  }
}
