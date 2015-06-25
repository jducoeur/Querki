package querki.session

import scala.concurrent.Future

import models.{OID, Thing, ThingId}

import querki.globals._

import querki.session.messages.{MarcoPoloItem, MarcoPoloResponse}

class MarcoPoloImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) {
  
  lazy val Core = interface[querki.core.Core]
  lazy val Links = interface[querki.links.Links]
  lazy val Tags = interface[querki.tags.Tags]
  
  // MarcoPoloImpl behaves differently from everything else. (This should eventually get fixed, but
  // that requires rewriting the client side.)
  def doRoute(req:Request):Future[String] = ???
  
  def handleMarcoPoloRequest(propIdOpt:Option[ThingId], q:String):MarcoPoloResponse = {
    implicit val s = state
    val lowerQ = q.toLowerCase()
    val propOpt = propIdOpt.flatMap(state.prop(_))
    val things = getLinksFromSpace(state, propOpt, lowerQ)
    val allItems:Seq[MarcoPoloItem] = propOpt match {
      case Some(prop) => {
        if (prop.pType == Core.LinkType) {
          things
        } else {
          // It's a Tag Type, so add Tags
          val tags = 
            Tags.fetchTags(state, prop).
              filter(_.toLowerCase().contains(lowerQ)).
              toList
          // Since it's a Tag Property, we don't want the OIDs
          val thingNames = things.map(_.display)
          // Strip out duplicates:
          val allNames = tags ++ thingNames.diff(tags)
          allNames.map(name => MarcoPoloItem(name, name))
        }
      }
      // Note that it only makes sense to omit the propId if you are collecting Links;
      // Tags depend on the prop.
      case _ => things
    }
    val itemsSorted = allItems.sortBy(_.display)
    
    MarcoPoloResponse(itemsSorted)
  }
    
  /**
   * Fetch all of the Things in the Space that are candidates for this property.
   * 
   * This takes the SpaceState as a parameter, so that it can recurse up the App tree.
   */
  private def getLinksFromSpace(spaceIn:SpaceState, propOpt:Option[AnyProp], lowerQ:String):Seq[MarcoPoloItem] = {
    implicit val space = spaceIn
    
    val things = {
      val allThings = space.allThings.toSeq
    
      // Filter the options if there is a valid Link Model:
      val thingsFiltered = {
        val filteredOpt = for {
          prop <- propOpt
          linkModelProp <- prop.getPropOpt(Links.LinkModelProp);
          targetModel <- linkModelProp.firstOpt
        }
          yield allThings.filter(_.isAncestor(targetModel))
          
        filteredOpt.getOrElse(allThings)
      }
    
      thingsFiltered.map(t => MarcoPoloItem(t.displayName, t.id.toThingId)).filter(_.display.toLowerCase().contains(lowerQ))
    }
    
    space.app match {
      case Some(app) => things ++ getLinksFromSpace(app, propOpt, lowerQ)
      case None => things
    }
  }
}