package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

class MenuBar(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[PageManager]
  
  def spaceOpt = DataAccess.space
  def space = spaceOpt.get
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
  
  type Call = String
  val emptyCall = ""
  
  trait Navigable
  
  case class NavSection(val title:String, val links:Seq[Navigable]) extends Navigable

  /**
   * Represents a single link to be shown in a menu.
   */
  case class NavLink(display:String, url:Call, id:Option[String] = None, enabled:Boolean = true) extends Navigable

  case object NavDivider extends Navigable
  
  def spacePath(pageName:String, params:(String,String)*):Call = {
    s"/u/${space.ownerHandle}/${space.urlName}/$pageName" + 
      (if (params.size > 0)
        s"?${params.map(pair => s"${pair._1}=${pair._2}").mkString("&")}"
      )
  }
  
  def thingIdPath(pageName:String):Call = {
    spacePath(pageName, ("thingId" -> thingOpt.get.urlName))
  }
  
  def adminPath(pageName:String):Call = {
    s"/admin/$pageName"
  }
  
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
      NavLink(truncateName(t.displayName), spacePath(t.urlName))
    }
  }
  
  def spaceLinks:Option[Seq[Navigable]] = {
    spaceOpt.map { space =>
      Seq(
        // TODO: these first two currently hook into Javascript. They should instead be direct callbacks to Scala:
        NavLink("Design a Model", emptyCall, Some("designModel")),
        NavLink("Create any Thing", emptyCall, Some("createThing")),
        NavLink("Add a Property", spacePath("createProperty")),
        NavLink("Show all Things", spacePath("All-Things")),
        NavLink("Show all Properties", spacePath("All-Properties")),
        NavLink("Sharing and Security", spacePath("_sharing"), enabled = DataAccess.request.isOwner)        
      )
    }
  }
  
  def thingLinks:Option[Seq[Navigable]] = {
    thingOpt.map { thing =>
      Seq(
        NavDivider,
        NavLink("Edit " + thing.displayName, thingIdPath("edit"), enabled = thing.isEditable),
        NavLink("View Source", thingIdPath("view")),
        NavLink("Advanced...", thingIdPath("_advanced")),
        NavLink("Explore...", thingIdPath("_explorer"), enabled = thing.isEditable),
        NavLink("Delete " + thing.displayName, emptyCall, Some("deleteThing"), enabled = thing.isDeleteable))      
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
        NavLink("Manage Users", adminPath("manageUsers")),
        NavLink("Show Space Status", adminPath("showSpaceStatus")),
        NavLink("Send System Message", adminPath("sendSystemMessage"))
      )))
    else
      None
  }
  
  //////////////////////////////////////
  
  def displayNavLink(display:String, url:Call, idStr:String, enabled:Boolean) = {
    if (enabled) {
      li(
        a(href:=url,
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

  def strippedTitle(title:String) = title.filter(c => c.isLetterOrDigit || c == ' ')

  /**
   * Displays a NavSection == that is, a single menu.
   */
  def displayNavSection(title:String, links:Seq[Navigable]):Frag = {
    val stripped = strippedTitle(title)
    
    li(cls:="dropdown",
      // The clickable drop-down head of the menu
      a(cls:="dropdown-toggle",
        data("target"):=s"#$stripped",
        href:=s"#$stripped",
        data("toggle"):="dropdown",
        title + " ",
        b(cls:="caret")
      ),
      // The menu itself
      ul(cls:="dropdown-menu",
        role:="menu",
        for (link <- links)
          displayNavigable(link)
      )
    )
}

  def displayNavigable(section:Navigable) = {
    section match {
      case NavLink(display, url, id, enabled) => {
        val idStr = id match {
          case Some(i) => " id=" + i
          case None => ""
        }
        displayNavLink(display, url, idStr, enabled)
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
                    displayNavigable(section)
                )
              )
            )
          )
        )
      )
}
