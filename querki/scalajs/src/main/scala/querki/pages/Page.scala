package querki.pages

import scala.concurrent.{Future, Promise}

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import querki.globals._

import querki.comm._
import querki.data.ThingInfo
import querki.display.{Gadget, WrapperDiv}
  
case class PageContents(title:String, content:TypedTag[dom.HTMLDivElement])

abstract class Page(e:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  implicit val ecology = e
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[Pages]
  
  /**
   * The contents of this page. Concrete subclasses must fill this in.
   */
  def pageContent:Future[PageContents]
  
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
  
  def doRender() = {
    val renderedContent = new WrapperDiv
    
    val outerPage = div(cls:="guts container-fluid",
      div(cls:="row",
        div(cls:="querki-content col-md-12",
          // If there is a message to flash, show it:
          Pages.getFlash.map { pair =>
            val (isError, msg) = pair
            div(
              classes(Seq("alert", if (isError) "alert-error" else "alert-info")),
              button(tpe:="button", cls:="close", data("dismiss"):="alert", "x"),
              if (isError) {
                strong("Error: ")
              },
              msg
            )
          },
          
          // The link to the Space:
          DataAccess.space match {
            case Some(space) =>
              div(cls:="_smallSubtitle _spaceLink _noPrint",
                a(href:=thingUrl(DataAccess.spaceId), space.displayName)
              )
            case None => div(cls:="_smallSubtitle _spaceLink _noPrint", raw("&nbsp;"))
          },
          
          // TODO: replace this with something prettier:
          renderedContent(p("Loading..."))
        )
      )
    )

    pageContent.foreach { content =>
      val fullyRendered = content.content.render
      renderedContent.replaceContents(fullyRendered)
      PageManager.update(content.title)
      InputGadgets.hookPendingGadgets()
      renderedContentPromise.success(fullyRendered)
      PageManager.onPageRendered(this)
    }
    
    outerPage
  }
}
