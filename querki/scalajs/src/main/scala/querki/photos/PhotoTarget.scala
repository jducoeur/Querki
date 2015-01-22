package querki.photos

import org.scalajs.dom

import querki.globals._

import querki.display.input.InputGadget

class PhotoTarget(implicit e:Ecology) extends InputGadget[dom.HTMLImageElement](e) {
  
  lazy val Pages = interface[querki.pages.Pages]
  
  // At least for now, this just wraps incoming images:
  def doRender() = ???
  def values = ???
  
  lazy val fromProp = $(elem).data("fromprop")
  
  def hook() = {
    println("Finding the page")
    val myPage = Pages.findPageFor(this)
    println(s"Got Page $myPage")
  }
}
