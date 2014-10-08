package querki.session

import models.{OID, Thing, ThingId}

import querki.globals._

import querki.session.messages.{MarcoPoloItem, MarcoPoloResponse}

trait MarcoPoloImpl extends SessionApiImpl {
  
  def Core:querki.core.Core
  lazy val Links = interface[querki.links.Links]
  def Tags:querki.tags.Tags
  
  def handleMarcoPoloRequest(propId:ThingId, q:String):MarcoPoloResponse = {
    implicit val s = state
    val lowerQ = q.toLowerCase()
    val prop = state.prop(propId).getOrElse(throw new Exception(s"handleMarcoPoloRequest got unknown propId $propId"))
    val things = getLinksFromSpace(state, prop, lowerQ)
    val allItems = 
      if (prop.pType == Core.LinkType) {
        things
      } else {
        // It's a Tag Type, so add Tags
        val tags = 
          Tags.fetchTags(state, prop).
            filter(_.toLowerCase().contains(lowerQ)).
            toList.
            map(tag => MarcoPoloItem(tag, tag))
        tags ++ things.diff(tags)
      }
    val itemsSorted = allItems.sortBy(_.display)
    
    MarcoPoloResponse(itemsSorted)
  }
    
  /**
   * Fetch all of the Things in the Space that are candidates for this property.
   * 
   * This takes the SpaceState as a parameter, so that it can recurse up the App tree.
   */
  private def getLinksFromSpace(spaceIn:SpaceState, prop:AnyProp, lowerQ:String):Seq[MarcoPoloItem] = {
    implicit val space = spaceIn
    
    val things = {
      val allThings = space.allThings.toSeq
    
      // Filter the options if there is a valid Link Model:
      val thingsFiltered = {
        val filteredOpt = for (
          linkModelProp <- prop.getPropOpt(Links.LinkModelProp);
          targetModel <- linkModelProp.firstOpt
            )
          yield allThings.filter(_.isAncestor(targetModel))
          
        filteredOpt.getOrElse(allThings)
      }
    
      thingsFiltered.map(t => MarcoPoloItem(t.displayName, t.id.toString)).filter(_.display.toLowerCase().contains(lowerQ))
    }
    
    space.app match {
      case Some(app) => things ++ getLinksFromSpace(app, prop, lowerQ)
      case None => things
    }
  }
}