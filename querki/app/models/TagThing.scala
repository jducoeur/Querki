package models

import controllers.RequestContext

import models.system.NameType

/**
 * This is essentially a pseudo-Thing, produced when you navigate to an unknown Name. It basically
 * exists to support the display of the space._showUnknownName.
 */
case class TagThing(name:String, space:SpaceState) extends Thing(UnknownOID, space.id, UnknownOID, Kind.Thing, () => Thing.emptyProps) {

  override def displayName = NameType.toDisplay(name)
  override def canonicalName = Some(name)
  
  override def render(implicit rc:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
    space.renderUnknownName(rc, name)
  }
}
object TagThing {
  val defaultDisplayText = """Referenced from:
[[_tagRefs -> _bulleted]]"""  
}