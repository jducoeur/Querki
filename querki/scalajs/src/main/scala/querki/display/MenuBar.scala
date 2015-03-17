package querki.display

import scala.scalajs.js
import org.scalajs.dom.{raw => dom}

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.display.input.InputGadget
import querki.notifications.NotifierGadget
import querki.search.SearchGadget

/**
 * The Gadget that renders and manages the Menu Bar.
 * 
 * TBD: this is arguably more coupled than it should be. Ideally, each subsystem would register its menu
 * items here, and this wouldn't have to know about all of them. The only issue is, how do we manage the
 * overall ordering of the list?
 */
class MenuBar(implicit e:Ecology) extends InputGadget[dom.HTMLDivElement](e) with QuerkiUIUtils {
  
  def values = ???
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val PageManager = interface[PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val Print = interface[querki.print.Print]
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
  
//  def thing(thingName:String) = controllers.Application.thing(ownerId, spaceId, thingName)
  def thing(thingName:String) = thingUrl(TID(thingName))
  
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
      NavLink(truncateName(t.displayName), thingUrl(t))
    }
  }
  
  def alwaysLinks:Seq[Navigable] = {
    Seq(
      NavLink("Refresh", onClick = Some({ () => PageManager.reload() }))
    )
  }
  
  def spaceLinks:Option[Seq[Navigable]] = {
    spaceOpt.map { space =>
      Seq(
        NavDivider,
        NavLink("Design a Model", onClick = Some({ () => DataModel.designAModel() })),
        NavLink("Create any Thing", onClick = Some({ () => DataModel.createAThing() })),
        NavLink("Show all Things", thing("All-Things")),
        NavLink("Show all Properties", thing("All-Properties")),
        NavLink("Sharing", Pages.sharingFactory.pageUrl(), enabled = DataAccess.request.isOwner)
      )
    }
  }
  
  def thingLinks:Option[Seq[Navigable]] = {
    thingOpt.map { thing =>
      val thingId = thing.urlName.underlying
      Seq(
        NavDivider,
        {
          if (thing.isModel)
            NavLink("Design " + thing.displayName, Editing.modelDesignerFactory.pageUrl(thing), enabled = thing.isEditable)
           else
            NavLink("Advanced Edit " + thing.displayName, Editing.advancedEditorFactory.pageUrl(thing), enabled = thing.isEditable)
        },
        NavLink("View Source", Pages.viewFactory.pageUrl(thing)),
        NavLink("Advanced...", Pages.advancedFactory.pageUrl(thing)),
        NavLink("Explore...", Pages.exploreFactory.pageUrl(thing)),
        NavLink("Print...", onClick = Some({ () => Print.print(thing)})),
        NavLink("Delete " + thing.displayName, enabled = thing.isDeleteable, onClick = Some({ () => DataModel.deleteAfterConfirm(thing) }))
      ) ++
      (if (thing.isModel)
        Seq(NavLink("Create a " + thing.displayName, Pages.createAndEditFactory.pageUrl(thing)))
       else
        Seq.empty)
    }
  }
  
  def actionSection = {
    val allSpaceLinks = spaceLinks.map { sl =>
      thingLinks match {
        case Some(tl) => sl ++ tl
        case None => sl
      }
    }
    val allLinks = alwaysLinks ++ allSpaceLinks.getOrElse(Seq.empty)
    Some(NavSection("Actions", allLinks))
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
          NavLink("Log in", controllers.Application.index())
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
        role:="button",
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
        div(cls:="navbar navbar-default navbar-fixed-top _noPrint",
          role:="navigation",
          div(cls:="container",
            div(cls:="navbar-header",  
              // This is the collapsed menu icon that we show on a small screen:
              button(tpe:="button", cls:="navbar-toggle",
                data("toggle"):="collapse",
                data("target"):=".querki-navbar-collapse",
                span(cls:="sr-only", "Toggle navigation"),
                span(cls:="icon-bar"),
                span(cls:="icon-bar"),
                span(cls:="icon-bar")
              ),
              
              // Show the logo on the left-hand side:
              a(cls:="navbar-brand",
                // TODO: where should we define this call?
                href:="/",
                img(src:=s"${PageManager.imagePath}/Logo-menubar.png")
              )
            ),
              
            div(cls:="querki-navbar-collapse navbar-collapse collapse",
              ul(cls:="nav navbar-nav",
                for (section <- sections)
                  yield displayNavigable(section)
              ),
                
              ul(cls:="nav navbar-nav navbar-right", displayNavigable(loginSection)),
                
              form(cls:="navbar-form navbar-right", role:="search",
                div(cls:="form-group",
                  new SearchGadget())),
                  
              if (UserAccess.user.isDefined) {
                ul(cls:="nav navbar-nav navbar-right",
                  li(new NotifierGadget)
                )
              }
            )
          )
        )
      )
      
  def hook() = {}
}
