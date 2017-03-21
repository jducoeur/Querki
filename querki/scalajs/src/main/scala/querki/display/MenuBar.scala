package querki.display

import scala.scalajs.js
import org.scalajs.{dom => fulldom}
import org.scalajs.dom.{raw => dom}

import scalatags.JsDom.all._

import org.querki.jquery._

import querki.api.StandardThings
import querki.comm._
import querki.globals._
import querki.identity.UserLevel
import querki.identity.skilllevel.{Complexity, SkillLevelsNeeded, UnspecifiedComplexity}
import querki.notifications.NotifierGadget
import querki.search.SearchGadget

/**
 * The Gadget that renders and manages the Menu Bar.
 * 
 * TBD: this is arguably more coupled than it should be. Ideally, each subsystem would register its menu
 * items here, and this wouldn't have to know about all of them. The only issue is, how do we manage the
 * overall ordering of the list?
 */
class MenuBar(std:StandardThings)(implicit e:Ecology) extends HookedGadget[dom.HTMLDivElement](e) 
  with QuerkiUIUtils with SkillLevelsNeeded 
{
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val Admin = interface[querki.admin.Admin]
  lazy val Apps = interface[querki.apps.Apps]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val History = interface[querki.history.History]
  lazy val PageManager = interface[PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val Print = interface[querki.print.Print]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  def spaceOpt = DataAccess.space
  def thingOpt = DataAccess.mainThing
  
  // There are a lot of items that are only displayed if the user have the Explore permission.
  lazy val hasExplore = spaceOpt.map { space =>
    space.permissions.contains(std.roles.canExplorePerm)
  }.getOrElse(false)
  
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
  
  case class NavSection(
    title:String, 
    links:Seq[Navigable], 
    index:Int, 
    icon:Option[String] = None, 
    iconOnly:Boolean = false,
    id:String = "") extends Navigable

  /**
   * Represents a single link to be shown in a menu.
   */
  case class NavLink(
    display:String, 
    url:URL = "#", 
    id:String = "", 
    enabled:Boolean = true, 
    onClick:Option[() => Unit] = None,
    newWindow:Boolean = false,
    requiresExplore:Boolean = false,
    complexity:Complexity = UnspecifiedComplexity,
    allowedDuringHistory:Boolean = false) extends Navigable

  case object NavDivider extends Navigable
  
  def thing(thingName:String) = thingUrl(TID(thingName))
  
  /**
   * Definition of the Menu Bar's data
   * 
   * @TODO: this is way the bloody heck too hard-coded. Can we come up with a decent way to
   * construct this based on the routes file, and reference it in a strongly-typed way, along the
   * lines of the old Server-side NavSection?
   */
  def sections:Seq[Navigable] = {
    Seq(actionSection, adminSection).flatten
  }
  
  def alwaysLinks:Seq[Navigable] = {
    Seq(
      NavLink("Refresh", id="_refreshMenuItem", onClick = Some({ () => PageManager.reload() }), allowedDuringHistory = true)
    )
  }
  
  def closeMiniMenu() = $(".querki-navbar-collapse").removeClass("in")
  
  def spaceLinks:Option[Seq[Navigable]] = {
    spaceOpt.map { space =>
      // Everything in this section at least implicitly requires Explore:
      if (hasExplore)
        Seq(
          NavDivider,
          NavLink(
            "Design a Model", 
            Editing.modelDesignerFactory.pageUrl(),
            id = "designAModel", 
            complexity = Standard,
            enabled = space.permissions.contains(std.security.canDesignPerm)),
          NavLink(
            "Create any Thing",
            Pages.createAndEditFactory.pageUrl(),
            id = "_createAnyThing", 
            complexity = Standard,
            enabled = space.permissions.contains(std.security.canCreatePerm)),
          NavLink("Show all Things", thing("All-Things"), complexity = Standard, allowedDuringHistory = true),
          NavLink("Show all Properties", thing("All-Properties"), complexity = Standard, allowedDuringHistory = true),
          NavLink("Sharing", Pages.sharingFactory.pageUrl(), id = "_sharingButton", enabled = DataAccess.request.isOwner),
          NavLink("Advanced...", 
              Pages.advancedFactory.pageUrl(thingOpt.getOrElse(space)), 
              id = "_openAdvancedItem", 
              requiresExplore = true, 
              complexity = Standard, 
              allowedDuringHistory = true)
        )
      else
        Seq.empty
    }
  }
  
  def thingLinks:Option[Seq[Navigable]] = {
    thingOpt.map { thing =>
      val thingId = thing.urlName.underlying
      Seq(
        NavDivider,
        {
          if (thing.isModel)
            NavLink(
              "Design " + thing.displayName, 
              Editing.modelDesignerFactory.pageUrl(thing), 
              enabled = thing.isEditable,
              requiresExplore = true,
              complexity = Standard)
           else
            NavLink(
              "Advanced Edit " + thing.displayName, 
              Editing.advancedEditorFactory.pageUrl(thing), 
              id = "_advEditButton",
              enabled = thing.isEditable,
              requiresExplore = true,
              complexity = Standard)
        },
        NavLink("View Source", Pages.viewFactory.pageUrl(thing), requiresExplore = true, complexity = Standard, allowedDuringHistory = true),
        NavLink(s"Security for ${thing.displayName}", Pages.securityFactory.pageUrl(thing), requiresExplore = true,
            enabled = DataAccess.request.isOwner,
            id = "_securityItem",
            complexity = Advanced),
        NavLink("Explore...", Pages.exploreFactory.pageUrl(thing), requiresExplore = true, complexity = Standard, allowedDuringHistory = true),
        NavLink("Print...", onClick = Some({ () => Print.print(thing)}), allowedDuringHistory = true),
        NavLink(
          "Delete " + thing.displayName, 
          enabled = thing.isDeleteable, 
          onClick = Some({ () => DataModel.deleteAfterConfirm(thing) }),
          requiresExplore = true)
      ) ++
      (if (thing.isModel)
        Seq(NavLink("Create a " + thing.displayName, Pages.createAndEditFactory.pageUrl(thing), requiresExplore = true))
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
    Some(NavSection("Actions", allLinks, 1100, id="_actionsMenu"))
  }
  
  def loginSection = {
    UserAccess.user match {
      case Some(user) => {
        if (user.actualUser) {
          NavSection(truncateName(user.mainIdentity.name), Seq(
            NavLink("Your Account", Pages.accountFactory.pageUrl()),
            NavLink(s"${SkillLevel.current.name} (change)", onClick = Some({ () => SkillLevel.changeSkillLevel() }), id="_skillLevelButton"),
            NavLink("Log out", controllers.LoginController.logout(), id="logout_button")
          ), 1900, id="_profile_menu")
        } else {
          // It's a guest:
          NavSection(truncateName(user.mainIdentity.name), Seq(
            NavLink("Log in / Sign up", onClick = Some({ () => UserAccess.login() })),
            NavLink("Log out of Guest", controllers.LoginController.logout(), id="logout_button")            
          ), 1900, id="_profile_menu")          
        }
      }
      case None => {
        NavSection("Not logged in", Seq(
          NavLink("Log in", onClick = Some({ () => UserAccess.login() }))
        ), 1900)
      }
    }
  }
  
  def adminSection = {
    if (UserLevel.isAdmin(DataAccess.request.userLevel))
      Some(NavSection("Admin", Seq(
        NavLink("Querki Statistics", Admin.statisticsFactory.pageUrl()),
        NavLink("Manage Users", Admin.manageUsersFactory.pageUrl()),
        NavLink("Show Space Status", Admin.monitorFactory.pageUrl()),
        NavLink("Debug Space Timing", Admin.spacesTimingFactory.pageUrl())
      ), 1300))
    else
      None
  }
  
  //////////////////////////////////////
  
  def displayNavLink(link:NavLink) = {
    link match {
      case NavLink(display, url, idStr, enabled, onClick, newWindow, hidden, complexity, allowedDuringHistory) => {
        if (enabled && (allowedDuringHistory || !History.viewingHistory)) {
          li(
            a(
              if (idStr.length > 0) id:=idStr,
              onClick.map { cb => href:=PageManager.currentHash }.getOrElse { href:=url },
              onClick.map { cb => 
                onclick:= { () =>
                  // If something doesn't actually change pages, we need to manually close the
                  // "miniMenu" -- the hamburger menu that shows on a narrow screen:
                  closeMiniMenu()
                  cb() 
                }
              },
              if (link.newWindow)
                target:="_blank",
              raw(display)
            )
          )
        } else {
          li(cls:="disabled",
            a(
              if (idStr.length > 0) id:=idStr,
              disabled:="disabled",
              raw(display))
          )
        }        
      }
    }

  }

  val displayNavDivider = li(cls:="divider")
  
  def filterLegal(s:String):String = {
    // This is more complex than one would like, because Char.isLetterOrDigit is unimplemented in
    // Scala.js. (Presumably because the proper Java definition requires handling all of Unicode?)
    s.toLowerCase.filter(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == ' ')
  }

  /**
   * Displays a NavSection == that is, a single menu.
   */
  def displayNavSection(section:NavSection):Frag = {
    section match {
      case NavSection(title, links, index, iconOpt, iconOnly, idStr) => {
        // Filter out characters that aren't legal in tag IDs, or the data-target will cause Bootstrap to choke:
        val legalTitle = filterLegal(title)
        val linkClass = s"dropdown-toggle"
        li(cls:="dropdown",
          tabindex:=index,
          // The clickable drop-down head of the menu
          a(cls:=linkClass,
            if (idStr.length > 0) id:=idStr,
            data("target"):=s"#$legalTitle",
            href:=s"#$legalTitle",
            data("toggle"):="dropdown",
            role:="button",
            iconOpt.map(classNames => i(cls:=classNames)),
            if (!iconOnly) title + " ",
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
    }
  }

  def displayNavigable(nav:Navigable) = {
    nav match {
      case link:NavLink => {
        if ((link.requiresExplore && !hasExplore) || !link.complexity.accepts(userSkillLevel))
          raw("")
        else
          displayNavLink(link)
      }
      case section:NavSection => displayNavSection(section)
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
                id:="_index_button",
                tabindex:=1000,
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
                
              // Search only makes sense in the context of a Space, at least for now:
              if (DataAccess.space.isDefined && hasExplore) {
                form(cls:="navbar-form navbar-right", role:="search",
                  tabindex:=1800,
                  div(cls:="form-group",
                    new SearchGadget()))
              },
                  
              if (UserAccess.isActualUser) {
                ul(cls:="nav navbar-nav navbar-right",
                  tabindex:=1700,
                  li(new NotifierGadget)
                )
              }
            )
          )
        )
      )
      
  def hook() = {}
}
