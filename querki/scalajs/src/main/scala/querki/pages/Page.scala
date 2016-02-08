package querki.pages

import scala.concurrent.{Future, Promise}

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import rx._
import org.querki.jsext._
import org.querki.jquery._

import querki.globals._

import querki.api.StandardThings
import querki.comm._
import querki.data.ThingInfo
import querki.display.{Gadget, WrapperDiv}
import querki.display.rx.{GadgetRef, RxDiv}
  
case class PageContents(title:String, content:TypedTag[dom.HTMLDivElement]) {  
  def titleOr(f: => String):String =
    if (title == "")
      f
    else
      title
}

object PageContents {
  def apply(content:TypedTag[dom.HTMLDivElement]):PageContents = PageContents("", content)  
}

abstract class Page(e:Ecology, pageName:String = "") extends Gadget[dom.HTMLDivElement] with EcologyMember with PageImplicits {
  
  implicit val ecology = e
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Localization = interface[querki.local.Localization]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[Pages]
  
  /**
   * The contents of this page. Concrete subclasses must fill this in.
   */
  def pageContent:Future[PageContents]
  
  private var _std:Option[StandardThings] = None
  def std = _std.get

  lazy val messages = Localization.messages("pages").getPackage(pageName)
  def msg(name:String, params:(String, String)*) = messages.msg(name, params:_*)
  // The standard location for a page's title is pages.name.pageTitle:
  lazy val pageTitle = msg("pageTitle")

  /**
   * This is the "ready" trigger -- we don't start rendering until the core libraries are in place.
   */
  def setStd:Future[Unit] = DataAccess.standardThings.map { s => _std = Some(s) }.flatMap { _ => Localization.ready }
  
  /**
   * This is an optional method that Pages can fill in, to refresh their content.
   * It is optional because it is Very Very Bad for certain pages (such as CreateThing)
   * to naively refresh.
   */
  def refresh():Unit = {}
  
  private val renderedContentPromise = Promise[dom.HTMLDivElement]
  /**
   * External tools can observe this; it will be fulfilled once this Page is
   * fully loaded and rendered. This may or may not be asynchronous, depending
   * on the page content.
   */
  val renderedContentFuture = renderedContentPromise.future
  
  /**
   * Miscellaneous metadata that other systems can store on the Page when appropriate.
   * This is deliberately unstructured.
   */
  private var metadata = Map.empty[String, Any]
  
  def storeMetadata(name:String, contents:Any):Unit = metadata += (name -> contents)
  def getMetadata(name:String):Option[Any] = metadata.get(name)
  
  /**
   * This is called before the Page begins to render. Pages should override it to do
   * things like register page-specific Gadget hooks. 
   */
  def beforeRender() = {}
  
  /**
   * Display the specified alert at the top of the page.
   * 
   * TBD: instead of isError, we should probably take an enumeration of the four standard Alert types from
   * Bootstrap: success, info, warning and danger.
   */
  def flash(isError:Boolean, msg:String) = {
    val newAlert:Gadget[_] = 
            div(
              classes(Seq("alert alert-dismissible", if (isError) "alert-danger" else "alert-info")),
              role:="alert",
              button(tpe:="button", cls:="close", data("dismiss"):="alert", aria.label:="Close", "x"),
              if (isError) {
                strong("Error: ")
              },
              msg
            )
    flashContents() = flashContents() :+ newAlert
  }
  lazy val flashContents = Var[Seq[Gadget[_]]](Seq.empty)
  lazy val flashDiv = new RxDiv(flashContents)
  
  lazy val renderSignal = GadgetRef.of[dom.HTMLSpanElement]
  
  def doRender() = {
    val renderedContent = new WrapperDiv
    
    val outerPage = div(cls:="guts container-fluid",
      div(cls:="row",
        div(cls:="querki-content col-md-12",
          // Placeholder for "flash" content in Play-speak (alert messages):
          flashDiv,
          
          // The link to the Space:
          DataAccess.space match {
            case Some(space) =>
              div(cls:="_smallSubtitle _spaceLink _noPrint",
                a(href:=thingUrl(DataAccess.spaceId), id:="_spaceLink", tabindex:=20000, space.displayName)
              )
            case None => div(cls:="_smallSubtitle _spaceLink _noPrint", raw("&nbsp;"))
          },
          
          // TODO: replace this with something prettier:
          renderedContent(p("Loading...")),
          
          renderSignal <= span()
        )
      )
    )

    setStd.flatMap(_ => pageContent).notYet.foreach { content =>
      val fullyRendered = content.content.render
      renderedContent.replaceContents(fullyRendered)
      PageManager.update(content.titleOr(pageTitle))
      Gadgets.hookPendingGadgets()
      reindex()
      renderedContentPromise.success(fullyRendered)
      PageManager.onPageRendered(this)
      // This is really just a signal to the test harness that we're done rendering:
      renderSignal <= span(id:="_pageRendered")
    }
    
    outerPage
  }
  
  // The guts of reindex. We're not doing this pure-recursively for fear of blowing the stack.
  private def reindex(e:dom.Element, from:Int):Int = {
    var i = from
    
    if (canFocus(e)) {
      $(e).attr("tabindex", i)
      i = i + 1        
    }
    
    $(e).children().foreach { child =>
      i = reindex(child, i)
    }
    i
  }
  
  /**
   * Pages can and should call this when they change the page layout. It adjusts the tab order to match current
   * reality.
   */
  def reindex():Unit = {
    elemOpt.foreach { e =>
      reindex(e, 20100)
    }
  }
}
