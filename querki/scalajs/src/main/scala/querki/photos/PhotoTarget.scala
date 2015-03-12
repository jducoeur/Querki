package querki.photos

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import querki.globals._

import querki.display.input.InputGadget

class PhotoTarget(implicit e:Ecology) extends InputGadget[dom.HTMLImageElement](e) {

  lazy val PhotosInternal = interface[PhotosInternal]
  
  // At least for now, this just wraps incoming images:
  def doRender() = ???
  def values = ???
  
  lazy val fromProp = $(elem).dataString("fromprop")
  
  def hook() = {
    PhotosInternal.recordTarget(this)
  }
  
  def displayFrom(thumbnail:Thumbnail) = {
    $(elem)
      .attr("src", thumbnail.fullSrc)
      .width(thumbnail.fullWidth)
      .height(thumbnail.fullHeight)
  }
}
