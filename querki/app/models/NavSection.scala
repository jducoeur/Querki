package models

import play.api.mvc.Call
import play.api.templates.Html

import controllers._

object NavSection {
  object homeNav extends NavSections(Seq())
  
  val maxNameDisplay = 25
    
  def truncateName(name:String) = {
    if (name.length < maxNameDisplay)
      name
    else {
      val cutoff = Math.max(name.lastIndexOf(" ", maxNameDisplay), 10)
      (name take cutoff) + "..."
    }
  }
  
  def loginNav(rc:RequestContext) = {
    rc.requester map { user =>
      // TODO: the Logged in as should eventually link to my profile/account:
      NavSection(NavLink("Logged in as " + user.name, routes.Application.spaces), Seq(
        NavLink("Your Spaces", routes.Application.spaces),
        NavLink("Log out", routes.Application.logout)
      ))
    } getOrElse {
      NavSection(NavLink("Not logged in", routes.Application.login), Seq(
        NavLink("Log in", routes.Application.login)
      ))
    }    
  }
      
  def nav(rc:RequestContext) = {
    def spaceId = rc.state.get.toThingId
    val owner = rc.ownerName
    
    val spaceSection = rc.state map { state =>
      NavSection(NavLink(truncateName(state.displayName), routes.Application.thing(owner, spaceId, spaceId)), Seq(
        NavLink("Create a Thing", routes.Application.createThing(owner, spaceId, None), Some("createThing")),
        NavLink("Add a Property", routes.Application.createProperty(owner, spaceId)),
        NavLink("Upload a Photo", routes.Application.upload(owner, spaceId)),
        NavLink("All Things", routes.Application.thing(owner, spaceId, "All+Things"))
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
      NavSection(NavLink(truncateName(thing.displayName), routes.Application.thing(owner, spaceId, thingId)), Seq(
        NavLink("Edit", routes.Application.editThing(owner, spaceId, thingId)),
        NavLink("Create a " + thing.displayName, routes.Application.createThing(owner, spaceId, Some(thingId))),
        NavLink("Export", routes.Application.exportThing(owner, spaceId, thingId))
      ) ++ attachment)
    }
    
    val sections = Seq(spaceSection, thingSection).flatten
    NavSections(sections)
  }
}

case class NavSections(sections:Seq[NavSection])

case class NavSection(val titleLink:NavLink, val links:Seq[NavLink])

case class NavLink(display:String, url:Call, id:Option[String] = None) {
  def idAttr:Html = Html(id match {
    case Some(i) => " id=\"" + i + "\" "
    case None => ""
  })
}
