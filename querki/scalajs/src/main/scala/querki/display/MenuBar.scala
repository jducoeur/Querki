package querki.display

import scala.scalajs.js
import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.notifications.NotifierGadget
import querki.search.SearchGadget

class MenuBar(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val PageManager = interface[PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  def spaceOpt = DataAccess.space
  def space = spaceOpt.get
  def ownerId = space.ownerHandle
  def spaceId = space.urlName
  def thingOpt = DataAccess.mainThing
  
  val maxNameDisplay = 25
  def truncateName(name:String) = {
    if (name.length < maxNameDisplay)
      name
    else {
      val cutoff = Math.max(name.lastIndexOf(" ", maxNameDisplay), 10)
      (name take cutoff) + "..."
    }
  }
  
  implicit def dyn2URL(rawCall:js.Dynamic):URL = {
    val call:PlayCall = rawCall
    call.url
  }
  
  /////////////////////////////////////
  //
  // Menu-defining local types
  //
  
  trait Navigable
  
  case class NavSection(val title:String, val links:Seq[Navigable]) extends Navigable

  /**
   * Represents a single link to be shown in a menu.
   */
  case class NavLink(display:String, url:URL = "#", id:Option[String] = None, enabled:Boolean = true, onClick:Option[() => Unit] = None) extends Navigable

  case object NavDivider extends Navigable
  
  def thing(thingName:String) = controllers.Application.thing(ownerId, spaceId, thingName)
  
  /**
   * Definition of the Menu Bar's data
   * 
   * @TODO: this is way the bloody heck too hard-coded. Can we come up with a decent way to
   * construct this based on the routes file, and reference it in a strongly-typed way, along the
   * lines of the old Server-side NavSection?
   */
  def sections:Seq[Navigable] = {
    Seq(thingLink, actionSection, adminSection).flatten
  }
  
  def thingLink = {
    thingOpt.map { t =>
      NavLink(truncateName(t.displayName), thing(t.urlName))
    }
  }
  
  def spaceLinks:Option[Seq[Navigable]] = {
    spaceOpt.map { space =>
      Seq(
        // TODO: these first two currently hook into Javascript. They should instead be direct callbacks to Scala:
        NavLink("Design a Model", PageManager.pageUrl("_design"), Some("designModel")),
        NavLink("Create any Thing", PageManager.pageUrl("_create"), Some("createThing")),
        NavLink("Show all Things", thing("All-Things")),
        NavLink("Show all Properties", thing("All-Properties")),
        NavLink("Sharing and Security", controllers.Application.sharing(ownerId, spaceId), enabled = DataAccess.request.isOwner)        
      )
    }
  }
  
  def thingLinks:Option[Seq[Navigable]] = {
    thingOpt.map { thing =>
      val thingId = thing.urlName
      Seq(
        NavDivider,
        NavLink("Edit " + thing.displayName, controllers.Application.editThing(ownerId, spaceId, thingId), enabled = thing.isEditable),
        NavLink("View Source", Pages.viewFactory.pageUrl("thingId" -> thingId)),
        NavLink("Advanced...", controllers.Application.showAdvancedCommands(ownerId, spaceId, thingId)),
        NavLink("Explore...", Pages.exploreFactory.pageUrl("thingId" -> thingId), enabled = thing.isEditable),
        NavLink("Delete " + thing.displayName, enabled = thing.isDeleteable, onClick = Some({ () => DataModel.deleteAfterConfirm(thing) }))
      )
    }
  }
  
  def actionSection = {
    val allLinks = spaceLinks.map { sl =>
      thingLinks match {
        case Some(tl) => sl ++ tl
        case None => sl
      }
    }
    allLinks.map(NavSection("Actions", _))
  }
  
  def loginSection = {
    UserAccess.user match {
      case Some(user) => {
        NavSection("Logged in as " + truncateName(user.mainIdentity.name), Seq(
          // TODO: make this a client page:
          NavLink("Your Profile", controllers.LoginController.userByName(user.mainIdentity.handle)),
          NavLink("Log out", controllers.LoginController.logout())
        ))  
      }
      case None => {
        NavSection("Not logged in", Seq(
          NavLink("Log in", controllers.Application.index)
        ))
      }
    }
  }
  
  def adminSection = {
    if (DataAccess.request.isAdmin)
      Some(NavSection("Admin", Seq(
        NavLink("Manage Users", controllers.AdminController.manageUsers()),
        NavLink("Show Space Status", controllers.AdminController.showSpaceStatus()),
        NavLink("Send System Message", controllers.AdminController.sendSystemMessage())
      )))
    else
      None
  }
  
  //////////////////////////////////////
  
  def displayNavLink(display:String, url:URL, idStr:String, enabled:Boolean, onClick:Option[() => Unit]) = {
    if (enabled) {
      val link = onClick match {
        case Some(cb) => {
          a(id:=idStr,
            href:=PageManager.currentHash,
            onclick:=cb,
            display)
        }
        case _ => {
          a(href:=url,
            id:=idStr,
            display)
        }
      }
      li(link)
    } else {
      li(cls:="disabled",
        a(display)
      )
    }
  }

  val displayNavDivider = li(cls:="divider")

  /**
   * Displays a NavSection == that is, a single menu.
   */
  def displayNavSection(title:String, links:Seq[Navigable]):Frag = {
    li(cls:="dropdown",
      // The clickable drop-down head of the menu
      a(cls:="dropdown-toggle",
        data("target"):=s"#$title",
        href:=s"#$title",
        data("toggle"):="dropdown",
        title + " ",
        b(cls:="caret")
      ),
      // The menu itself
      ul(cls:="dropdown-menu",
        role:="menu",
        for (link <- links)
          yield displayNavigable(link)
      )
    )
  }

  def displayNavigable(section:Navigable) = {
    section match {
      case NavLink(display, url, id, enabled, onClick) => {
        val idStr = id match {
          case Some(i) => " id=" + i
          case None => ""
        }
        displayNavLink(display, url, idStr, enabled, onClick)
      }
      case NavSection(title, links) => displayNavSection(title, links)
      case NavDivider => displayNavDivider
    }
  }
  
  def doRender() =
      div(cls:="container",
        div(cls:="navbar navbar-fixed-top _noPrint",
          div(cls:="navbar-inner",
            div(cls:="container",
              
              // This is the collapsed menu icon that we show on a small screen:
              a(cls:="btn btn-navbar",
                data("toggle"):="collapse",
                data("target"):=".nav-collapse",
                span(cls:="icon-bar"),
                span(cls:="icon-bar"),
                span(cls:="icon-bar")
              ),
              
              // Show the logo on the left-hand side:
              a(cls:="brand",
                // TODO: where should we define this call?
                href:="/",
                img(src:=s"${PageManager.imagePath}/Logo-menubar.png")
              ),
              
              div(cls:="nav-collapse collapse",
                ul(cls:="nav",
                  for (section <- sections)
                    yield displayNavigable(section)
                ),
                
                ul(cls:="nav pull-right", displayNavigable(loginSection)),
                
                form(cls:="navbar-search pull-right",
                  new SearchGadget()),
                  
                if (UserAccess.user.isDefined) {
                  ul(cls:="nav pull-right",
                    li(new NotifierGadget)
                  )
                }
              )
            )
          )
        )
      )
}
