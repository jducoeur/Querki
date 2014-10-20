package querki.pages

import scala.concurrent.Future

import org.scalajs.dom
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import querki.globals._

import querki.comm._
import querki.data.ThingInfo
import querki.display.{Gadget, WrapperDiv}

abstract class Page(e:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  implicit val ecology = e
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[querki.display.PageManager]
  
  /**
   * Shortcut for fetching the URL of a Thing.
   */
  def thingUrl(thing:ThingInfo)(implicit ecology:Ecology) = {
    controllers.Application.thing.spaceUrl(thing.urlName)
  }
  
  case class PageContents(title:String, content:TypedTag[dom.HTMLDivElement])
  
  /**
   * The contents of this page. Concrete subclasses must fill this in.
   */
  def pageContent:Future[PageContents]
  
  def doRender() = {
    val renderedContent = new WrapperDiv
    
    val outerPage = div(cls:="guts container-fluid",
      div(cls:="row-fluid",
        div(cls:="querki-content span12",
          // The link to the Space:
          DataAccess.space match {
            case Some(space) =>
              div(cls:="_smallSubtitle _spaceLink _noPrint",
                // TODO: the hashing shouldn't be hard-coded here:
                a(href:=s"#${DataAccess.spaceId}"/*controllers.Application.thing.spaceUrl(DataAccess.spaceId)*/, space.displayName)
              )
            case None => div(cls:="_smallSubtitle _spaceLink _noPrint", raw("&nbsp;"))
          },
          // TODO: replace this with something prettier:
          renderedContent(p("Loading..."))
        )
      )
    )

    pageContent.foreach { content =>
      renderedContent.replaceContents(content.content.render)
      PageManager.update(content.title)
    }
    
    outerPage
  }
}
