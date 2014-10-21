package querki.pages

import scala.concurrent.{Future, Promise}

import org.scalajs.dom
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
  lazy val PageManager = interface[querki.display.PageManager]
  
  /**
   * Shortcut for fetching the URL of a Thing.
   */
  def thingUrl(thing:ThingInfo)(implicit ecology:Ecology):String = {
    thingUrl(thing.urlName)
  }
  
  def thingUrl(name:String) = s"#$name"
  
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
  
  def doRender() = {
    val renderedContent = new WrapperDiv
    
    val outerPage = div(cls:="guts container-fluid",
      div(cls:="row-fluid",
        div(cls:="querki-content span12",
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
      renderedContentPromise.success(fullyRendered)
    }
    
    outerPage
  }
}
