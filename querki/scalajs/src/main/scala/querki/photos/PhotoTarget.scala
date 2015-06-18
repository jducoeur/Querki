package querki.photos

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import querki.globals._

import querki.display.HookedGadget

class PhotoTarget(implicit e:Ecology) extends HookedGadget[dom.HTMLImageElement](e) {

  lazy val PhotosInternal = interface[PhotosInternal]
  
  // At least for now, this just wraps incoming images:
  def doRender() = ???
  
  lazy val fromProp = $(elem).dataString("fromprop")
  
  def hook() = {
    PhotosInternal.recordTarget(this)
  }
  
  def displayFrom(thumbnail:Thumbnail) = {
    $(elem)
      .attr("src", thumbnail.fullSrc)
  }
}
