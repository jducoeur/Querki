package controllers

import play.api.mvc.Call
import querki.ecology._
import scala.reflect.runtime.universe

trait Navigable

case class NavSections(sections:Seq[Navigable])

case class NavSection(val title:String, val links:Seq[Navigable]) extends Navigable

/**
 * Represents a single link to be shown in a menu.
 * 
 * IMPORTANT: the display is taken as literal HTML, and is not further escaped! Be very sure that anything
 * you use for the display parameter has been properly HTML-neutered! 
 */
case class NavLink(display:String, url:Call, id:Option[String] = None, enabled:Boolean = true) extends Navigable

case object NavDivider extends Navigable

trait NavSectionMgr extends EcologyInterface {
  def nav(rc:PlayRequestContext):NavSections
  def loginNav(rc:PlayRequestContext):NavSection
}

/**
 * Note that this is a weird case, an Ecot in controllers. But it clearly *is* an Ecot -- it
 * is a bundle of static logic that needs to work with the Ecology -- and it needs controller
 * capabilities such as routes. So it lives here for the time being.
 */
class NavSectionEcot(e:Ecology) extends QuerkiEcot(e) with NavSectionMgr {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  
  object homeNav extends NavSections(Seq())
  
  val maxNameDisplay = 25
  
  val emptyCall = Call("GET", "#")
    
  def truncateName(name:String) = {
    if (name.length < maxNameDisplay)
      name
    else {
      val cutoff = Math.max(name.lastIndexOf(" ", maxNameDisplay), 10)
      (name take cutoff) + "..."
    }
  }
  
  def loginNav(rc:PlayRequestContext) = {
    rc.requester map { user =>
      NavSection("Logged in as " + truncateName(user.name), Seq(
        NavLink("Your Spaces", routes.Application.spaces),
        NavLink("Your Profile", routes.LoginController.userByName(user.mainIdentity.handle)),
        NavLink("Log out", routes.LoginController.logout)
      ))
    } getOrElse {
      NavSection("Not logged in", Seq(
        NavLink("Log in", routes.LoginController.login)
      ))
    }    
  }
  
  def deletable(t:Thing, rc:PlayRequestContext):Boolean = {
    rc.interface[querki.datamodel.DataModelAccess].isDeletable(t)(rc.state.get)
  }
      
  def nav(rc:PlayRequestContext) = {
    def spaceId = rc.state.get.toThingId
    val owner = rc.ownerHandle
    // For menu purposes, don't duplicate the space if it's the Thing:
    val thingIsSpace = rc.thing.isDefined && (rc.thing.get.id == rc.state.get.id)
    val actualThing = rc.thing.flatMap(t => if (thingIsSpace) None else Some(t))
    
    val spaceSection = rc.state map { state =>
      NavLink(truncateName(state.displayName), routes.Application.thing(owner, spaceId, spaceId))
    }
    
    val thingSection = actualThing map { thing =>
      val thingId = thing.toThingId
      NavLink(truncateName(thing.displayName), routes.Application.thing(owner, spaceId, thingId))
    }
    
    val spaceLinksOpt = rc.state map { state =>
      Seq(
        NavLink("Create any Thing", emptyCall, Some("createThing")),
        NavLink("Add a Property", routes.Application.createProperty(owner, spaceId)),
        NavLink("Upload a Photo", routes.Application.upload(owner, spaceId)),
        NavLink("Show all Things", routes.Application.thing(owner, spaceId, "All+Things")),
        NavLink("Show all Properties", routes.Application.thing(owner, spaceId, "All+Properties")),
        NavLink("Sharing and Security", routes.Application.sharing(owner, spaceId), enabled = rc.isOwner)
      )
    }
    val spaceLinks = spaceLinksOpt.getOrElse(Seq.empty[NavLink])
    val thingLinksOpt = rc.thing map { thing =>
      val thingId = thing.toThingId
      def attachment:Option[NavLink] = {
        thing.kind match {
          case models.Kind.Attachment => Some(NavLink("Download", routes.Application.attachment(owner, spaceId, thingId)))
          case _ => None
        }
      }
      def create:Option[NavLink] = {
        if (thingIsSpace)
          None
        else
          Some(NavLink("Create a " + thing.displayName, routes.Application.createThing(owner, spaceId, Some(thingId))))
      }
      Seq(
        NavDivider,
        NavLink("Edit " + thing.displayName, routes.Application.editThing(owner, spaceId, thingId), None, rc.state.get.canEdit(rc.requesterOrAnon, thing.id)),
        NavLink("View Source", routes.Application.viewThing(owner, spaceId, thingId), None, AccessControl.canRead(rc.state.get, rc.requesterOrAnon, thing.id)),
        NavLink("Advanced...", routes.Application.showAdvancedCommands(owner, spaceId, thingId), None, AccessControl.canRead(rc.state.get, rc.requesterOrAnon, thing.id)),
        NavLink("Explore...", routes.ExploreController.showExplorer(owner, spaceId, thingId), None, rc.state.get.canEdit(rc.requesterOrAnon, thing.id)),
        // Note that the following route is bogus: we actually navigate in Javascript, after verifying they want to delete:
        NavLink("Delete " + thing.displayName, routes.Application.thing(owner, spaceId, thingId), Some("deleteThing"), deletable(thing, rc))
      ) ++ create ++ attachment
    }
    val thingLinks = thingLinksOpt.getOrElse(Seq.empty[NavLink])
    val actionLinks = spaceLinks ++ thingLinks
    val actionSection =
      if (actionLinks.isEmpty)
        None
      else
        Some(NavSection("Actions", actionLinks))
        
    val adminSection =
      if (rc.requesterOrAnon.isAdmin) {
        Some(NavSection("Admin", Seq(
          NavLink("Manage Users", routes.AdminController.manageUsers)
        )))
      } else
        None
    
    val sections = Seq(spaceSection, thingSection, actionSection, adminSection).flatten
    NavSections(sections)
  }
}
