package models

import models.system.OIDs
import models.system.{ExactlyOne, LinkModelProp, ShowUnknownProp}
import models.system.{NameType, NewTagSetType, PlainText, PlainTextType, TagSetType}

import querki.ecology._
import querki.util.SafeUrl
import querki.values.{QLContext, RequestContext, SpaceState}

/**
 * This is essentially a pseudo-Thing, produced when you navigate to an unknown Name. It basically
 * exists to support the display of the Undefined Tag View.
 */
case class TagThing(name:String, space:SpaceState) extends Thing(UnknownOID, space.id, UnknownOID, Kind.Thing, () => Thing.emptyProps, querki.time.epoch) {

  override lazy val displayName = name
  override lazy val canonicalName = Some(name)
  override lazy val toThingId:ThingId = new AsDisplayName(name)
  
  override def render(implicit rc:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
    import ql._
    
    implicit val s = space
    val model = TagThing.preferredModelForTag(space, name)
    val propAndValOpt = model.getPropOpt(ShowUnknownProp) orElse space.getPropOpt(ShowUnknownProp)
    val nameVal = ExactlyOne(PlainTextType(name))
    val nameAsContext = QLContext(nameVal, Some(rc))
    // TODO: the link below shouldn't be so hard-coded!
    propAndValOpt.map(pv => pv.render(nameAsContext)).getOrElse(Wikitext(name + " doesn't exist yet. [Click here to create it.](edit?thingId=" + SafeUrl(name) + ")"))    
  }
}
object TagThing {
  val defaultDisplayText = """Referenced from:
[[_tagRefs -> _bulleted]]"""  
    
  lazy val Basic = getInterface[querki.basic.Basic]
    
  def preferredModelForTag(implicit state:SpaceState, nameIn:String):Thing = {
    val tagProps = state.propsOfType(TagSetType).filter(_.hasProp(OIDs.LinkModelOID))
    val newTagProps = state.propsOfType(NewTagSetType).filter(_.hasProp(OIDs.LinkModelOID))
    val name = NameType.canonicalize(nameIn)
    val plainName = PlainText(nameIn)
    if (tagProps.isEmpty && newTagProps.isEmpty)
      Basic.SimpleThing
    else {
      val candidates = state.allThings.toSeq
    
      // Find the first Tag Set property (if any) that is being used with this Tag:
      val tagPropOpt:Option[Property[_,_]] = tagProps.find { prop =>
        val definingThingOpt = candidates.find { thing =>
          val propValOpt = thing.getPropOpt(prop)
          propValOpt.map(_.contains(name)).getOrElse(false)
        }
        definingThingOpt.isDefined
      }
      
      val newTagPropOpt:Option[Property[_,_]] = newTagProps.find { prop =>
        val definingThingOpt = candidates.find { thing =>
          val propValOpt = thing.getPropOpt(prop)
          propValOpt.map(_.contains(plainName)).getOrElse(false)
        }
        definingThingOpt.isDefined
      }
      
      val definingPropOpt = newTagPropOpt orElse tagPropOpt
      
      // Get the Link Model for that property:
      val modelOpt = 
        for (
          tagProp <- definingPropOpt;
          linkModelPropVal <- tagProp.getPropOpt(LinkModelProp);
          modelId <- linkModelPropVal.firstOpt;
          model <- state.anything(modelId)
          )
          yield model

      modelOpt.getOrElse(Basic.SimpleThing)
    }
  }
}