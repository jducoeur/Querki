package querki.photos

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import org.querki.facades.bootstrap._

import querki.globals._

import querki.display.input.InputGadget

class Thumbnail(implicit e:Ecology) extends InputGadget[dom.HTMLImageElement](e) {
  
  lazy val PhotosInternal = interface[PhotosInternal]

  // At least for now, this just wraps incoming thumbnails:
  def doRender() = ???
  def values = ???
  
  lazy val fullSrc = $(elem).dataString("fullsrc")
  lazy val fullWidth = $(elem).data("fullwidth").asInstanceOf[Int]
  lazy val fullHeight = $(elem).data("fullheight").asInstanceOf[Int]
  lazy val fromProp = $(elem).dataString("fromprop")
  
  def showFull() = {
    PhotosInternal.findTargetFor(this) match {
      case Some(target) => target.displayFrom(this)
      // There is no target on the page for this Property's pictures, so pop a
      // dialog instead:
      case None => PhotosInternal.showInDialog(this)
    }
  }
  
  def hook() = {
    $(elem).click({ evt:JQueryEventObject =>
      showFull()
    })
    
    $(elem).tooltip(TooltipOptions.title("Click to see full sized"))
  }
}
