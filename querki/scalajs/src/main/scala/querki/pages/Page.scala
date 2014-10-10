package querki.pages

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.data.ThingInfo
import querki.display.{Gadget, WrapperDiv}

abstract class Page(e:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  implicit val ecology = e
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  type DetailsType <: PageDetails
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  // HACK: this downcast is ugly and a bit dangerous. Can we do this in a more typesafe way?
  lazy val details = DataAccess.request.pageDetails.asInstanceOf[DetailsType]
  
  /**
   * Shortcut for fetching the URL of a Thing.
   */
  def thingUrl(thing:ThingInfo)(implicit ecology:Ecology) = {
    controllers.Application.thing.spaceUrl(thing.urlName)
  }
  
  /**
   * The title of this page. Concrete subclasses must fill this in.
   */
  def title:String
  
  /**
   * The contents of this page. Concrete subclasses must fill this in.
   */
  def pageContent:Modifier
  
  val contentDiv = new WrapperDiv
  
  def replaceContents(newContent:dom.Element) = {
    contentDiv.replaceContents(newContent)
  }
  
  def doRender() = {
    div(cls:="guts container-fluid",
      div(cls:="row-fluid",
        contentDiv(cls:="querki-content span12",
          // The link to the Space:
          DataAccess.space match {
            case Some(space) =>
              div(cls:="_smallSubtitle _spaceLink _noPrint",
                a(href:=controllers.Application.thing.spaceUrl(DataAccess.spaceId), space.displayName)
              )
            case None => div(cls:="_smallSubtitle _spaceLink _noPrint", raw("&nbsp;"))
          },
          pageContent
        )
      )
    )
  }
}
