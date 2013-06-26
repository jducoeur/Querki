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
      NavSection("Logged in as " + user.name, Seq(
        NavLink("Your Spaces", routes.Application.spaces),
        NavLink("Log out", routes.Application.logout)
      ))
    } getOrElse {
      NavSection("Not logged in", Seq(
        NavLink("Log in", routes.Application.login)
      ))
    }    
  }
      
  def nav(rc:RequestContext) = {
    def spaceId = rc.state.get.toThingId
    val owner = rc.ownerName
    // For menu purposes, don't duplicate the space if it's the Thing:
    val actualThing = rc.thing.flatMap(t => if (t.id == rc.state.get.id) None else Some(t))
    
    val spaceSection = rc.state map { state =>
      NavLink(truncateName(state.displayName), routes.Application.thing(owner, spaceId, spaceId))
    }
    
    val thingSection = actualThing map { thing =>
      val thingId = thing.toThingId
      NavLink(truncateName(thing.displayName), routes.Application.thing(owner, spaceId, thingId))
    }
    
    val spaceLinksOpt = rc.state map { state =>
      Seq(
        NavLink("Create any Thing", routes.Application.createThing(owner, spaceId, None), Some("createThing")),
        NavLink("Add a Property", routes.Application.createProperty(owner, spaceId)),
        NavLink("Upload a Photo", routes.Application.upload(owner, spaceId)),
        NavLink("Show all Things", routes.Application.thing(owner, spaceId, "All+Things"))
      )
    }
    val spaceLinks = spaceLinksOpt.getOrElse(Seq.empty[NavLink])
    val thingLinksOpt = rc.thing map { thing =>
      val thingId = thing.toThingId
      def attachment:Option[NavLink] = {
        thing.kind match {
          case Kind.Attachment => Some(NavLink("Download", routes.Application.attachment(owner, spaceId, thingId)))
          case _ => None
        }
      }
      Seq(
        NavLink("Edit " + thing.displayName, routes.Application.editThing(owner, spaceId, thingId)),
        NavLink("Create a " + thing.displayName, routes.Application.createThing(owner, spaceId, Some(thingId)))
        /*,
        NavLink("Export " + thing.displayName, routes.Application.exportThing(owner, spaceId, thingId))*/
      ) ++ attachment
    }
    val thingLinks = thingLinksOpt.getOrElse(Seq.empty[NavLink])
    val actionLinks = spaceLinks ++ thingLinks
    val actionSection =
      if (actionLinks.isEmpty)
        None
      else
        Some(NavSection("Actions", actionLinks))
    
    val sections = Seq(spaceSection, thingSection, actionSection).flatten
    NavSections(sections)
  }
}

trait Navigable

case class NavSections(sections:Seq[Navigable])

case class NavSection(val title:String, val links:Seq[NavLink]) extends Navigable

case class NavLink(display:String, url:Call, id:Option[String] = None) extends Navigable {
  def idAttr:Html = Html(id match {
    case Some(i) => " id=\"" + i + "\" "
    case None => ""
  })
}
