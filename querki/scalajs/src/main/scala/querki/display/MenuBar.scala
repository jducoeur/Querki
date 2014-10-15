package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._

class MenuBar(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[PageManager]
  
  def spaceOpt = DataAccess.space
  def space = spaceOpt.get
  def userName = space.ownerHandle
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
  
  /////////////////////////////////////
  //
  // Menu-defining local types
  //
  
  trait Navigable
  
  case class NavSection(val title:String, val links:Seq[Navigable]) extends Navigable

  /**
   * Represents a single link to be shown in a menu.
   */
  case class NavLink(display:String, call:PlayCall, id:Option[String] = None, enabled:Boolean = true) extends Navigable

  case object NavDivider extends Navigable
  
  def thing(thingName:String) = controllers.Application.thing(userName, spaceId, thingName)
  
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
        NavLink("Design a Model", EmptyCall, Some("designModel")),
        NavLink("Create any Thing", EmptyCall, Some("createThing")),
        NavLink("Add a Property", controllers.Application.createProperty(userName, spaceId)),
        NavLink("Show all Things", thing("All-Things")),
        NavLink("Show all Properties", thing("All-Properties")),
        NavLink("Sharing and Security", controllers.Application.sharing(userName, spaceId), enabled = DataAccess.request.isOwner)        
      )
    }
  }
  
  def thingLinks:Option[Seq[Navigable]] = {
    thingOpt.map { thing =>
      val thingId = thing.urlName
      Seq(
        NavDivider,
        NavLink("Edit " + thing.displayName, controllers.Application.editThing(userName, spaceId, thingId), enabled = thing.isEditable),
        NavLink("View Source", controllers.Application.viewThing(userName, spaceId, thingId)),
        NavLink("Advanced...", controllers.Application.showAdvancedCommands(userName, spaceId, thingId)),
        NavLink("Explore...", controllers.ExploreController.showExplorer(userName, spaceId, thingId), enabled = thing.isEditable),
        NavLink("Delete " + thing.displayName, EmptyCall, Some("deleteThing"), enabled = thing.isDeleteable))      
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
  
  def displayNavLink(display:String, call:PlayCall, idStr:String, enabled:Boolean) = {
    if (enabled) {
      li(
        a(href:=call.url,
          id:=idStr,
          display)
      )
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
      case NavLink(display, call, id, enabled) => {
        val idStr = id match {
          case Some(i) => " id=" + i
          case None => ""
        }
        displayNavLink(display, call, idStr, enabled)
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
                
                form(cls:="navbar-search pull-right",
                  new SearchGadget())
              )
            )
          )
        )
      )
}
