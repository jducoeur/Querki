package models

import models.system.{ExactlyOne, NameType, ShowUnknownProp}

import modules.time.TimeModule

import querki.util.SafeUrl
import querki.values.{QLContext, RequestContext, SpaceState}

/**
 * This is essentially a pseudo-Thing, produced when you navigate to an unknown Name. It basically
 * exists to support the display of the space._showUnknownName.
 */
case class TagThing(name:String, space:SpaceState) extends Thing(UnknownOID, space.id, UnknownOID, Kind.Thing, () => Thing.emptyProps, TimeModule.epoch) {

  override lazy val displayName = NameType.toDisplay(name)
  override lazy val canonicalName = Some(name)
  
  override def render(implicit rc:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
    import ql._
    
    implicit val s = space
    val opt = space.getPropOpt(ShowUnknownProp)
    val nameVal = ExactlyOne(NameType(name))
    val nameAsContext = QLContext(nameVal, Some(rc))
    // TODO: the link below shouldn't be so hard-coded!
    opt.map(pv => pv.render(nameAsContext)).getOrElse(Wikitext(NameType.toDisplay(name) + " doesn't exist yet. [Click here to create it.](edit?thingId=" + SafeUrl(name) + ")"))    
  }
}
object TagThing {
  val defaultDisplayText = """Referenced from:
[[_tagRefs -> _bulleted]]"""  
}