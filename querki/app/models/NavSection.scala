package models

import play.api.mvc.Call

import controllers._

object NavSection {
  object homeNav extends NavSections(Seq(querkiSection))
  
  val maxNameDisplay = 25
    
  def truncateName(name:String) = {
    if (name.length < maxNameDisplay)
      name
    else {
      val cutoff = Math.max(name.lastIndexOf(" ", maxNameDisplay), 10)
      (name take cutoff) + "..."
    }
  }
      
  def nav(rc:RequestContext) = {
    def spaceId = rc.state.get.toThingId
    val owner = rc.ownerName
    
    val spaceSection = rc.state map { state =>
      NavSection(truncateName(state.displayName), Seq(
        NavLink("Space Home", routes.Application.space(owner, spaceId)),
        NavLink("All Things", routes.Application.things(owner, spaceId)),
        NavLink("Create a Thing", routes.Application.createThing(owner, spaceId, None)),
        NavLink("Add a Property", routes.Application.createProperty(owner, spaceId)),
        NavLink("Upload a Photo", routes.Application.upload(owner, spaceId))
      ))
    }
    
    val thingSection = rc.thing map { thing =>
      val thingId = thing.toThingId
      def attachment:Option[NavLink] = {
        thing.kind match {
          case Kind.Attachment => Some(NavLink("Download", routes.Application.attachment(owner, spaceId, thingId)))
          case _ => None
        }
      }
      NavSection(truncateName(thing.displayName), Seq(
        NavLink("Edit", routes.Application.editThing(owner, spaceId, thingId)),
        NavLink("Show", routes.Application.thing(owner, spaceId, thingId)),
        NavLink("Create a " + thing.displayName, routes.Application.createThing(owner, spaceId, Some(thingId))),
        NavLink("Export", routes.Application.exportThing(owner, spaceId, thingId))
      ) ++ attachment)
    }
    
    val loginSection = rc.requester map { user =>
      NavSection("Logged in as " + user.name, Seq(
        NavLink("Your Spaces", routes.Application.spaces),
        NavLink("Log out", routes.Application.logout)
      ))
    } getOrElse {
      NavSection("Not logged in", Seq(
        NavLink("Log in", routes.Application.login)
      ))
    }
    
    val sections = Seq(querkiSection) ++ spaceSection ++ thingSection
    NavSections(sections :+ loginSection)
  }
  
  val querkiSection = NavSection("Querki", Seq(
      NavLink("Home", routes.Application.index),
      NavLink("Your Spaces", routes.Application.spaces),
      NavLink("Logout", routes.Application.logout)
      ))
}

case class NavSections(sections:Seq[NavSection])

case class NavSection(val title:String, val links:Seq[NavLink])

case class NavLink(display:String, url:Call)
