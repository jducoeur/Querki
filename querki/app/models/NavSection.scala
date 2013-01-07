package models

import play.api.mvc.Call

import controllers._

object NavSection {
  object homeNav extends NavSections(Seq(querkiSection))
  
  def spaceNav(rc:RequestContext) = {
    val state = rc.state.get
    val id = state.toThingId
    val owner = rc.ownerName
    NavSections(Seq(
      NavSection("This Space", Seq(
        NavLink("Space Home", routes.Application.space(owner, id)),
        NavLink("All Things", routes.Application.things(owner, id)),
        NavLink("Create a Thing", routes.Application.createThing(owner, id)),
        NavLink("Add a Property", routes.Application.createProperty(owner, id)),
        NavLink("Upload a Photo", routes.Application.upload(owner, id))
      )),
      querkiSection
    ))
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
