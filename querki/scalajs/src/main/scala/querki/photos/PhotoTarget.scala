package querki.photos

import org.scalajs.dom

import querki.globals._

import querki.display.input.InputGadget

class PhotoTarget(implicit e:Ecology) extends InputGadget[dom.HTMLImageElement](e) {
  
  // At least for now, this just wraps incoming images:
  def doRender() = ???
  def values = ???
  
  lazy val fromProp = $(elem).data("fromprop")
  
  def hook() = {
    
  }
}