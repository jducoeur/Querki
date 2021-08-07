package controllers

import play.api.mvc.Call
import querki.ecology._
import scala.reflect.runtime.universe

trait Navigable

case class NavSections(sections: Seq[Navigable])

case class NavSection(
  val title: String,
  val links: Seq[Navigable]
) extends Navigable

/**
 * Represents a single link to be shown in a menu.
 *
 * IMPORTANT: the display is taken as literal HTML, and is not further escaped! Be very sure that anything
 * you use for the display parameter has been properly HTML-neutered!
 */
case class NavLink(
  display: String,
  url: Call,
  id: Option[String] = None,
  enabled: Boolean = true
) extends Navigable

object NavLink {

  def apply(
    display: String,
    url: String
  ): NavLink = NavLink(display, new Call("GET", url))
}

case object NavDivider extends Navigable

trait NavSectionMgr extends EcologyInterface {
  def loginNav(rc: PlayRequestContext): Navigable
}

/**
 * Note that this is a weird case, an Ecot in controllers. But it clearly *is* an Ecot -- it
 * is a bundle of static logic that needs to work with the Ecology -- and it needs controller
 * capabilities such as routes. So it lives here for the time being.
 *
 * TODO: I *think* this whole Ecot is now dead (we're not displaying this menu in the vestigial
 * top-level pages), so it can likely be deleted.
 */
class NavSectionEcot(e: Ecology) extends QuerkiEcot(e) with NavSectionMgr {
  val maxNameDisplay = 25

  def truncateName(name: String) = {
    if (name.length < maxNameDisplay)
      name
    else {
      val cutoff = Math.max(name.lastIndexOf(" ", maxNameDisplay), 10)
      (name.take(cutoff)) + "..."
    }
  }

  def loginNav(rc: PlayRequestContext): Navigable = {
    rc.requester.map { user =>
      NavSection(
        "Logged in as " + truncateName(user.name),
        Seq(
          NavLink("Log out", routes.LoginController.logout)
        )
      )
    }.getOrElse {
      NavLink("Log in", routes.ClientController.index)
    }
  }
}
