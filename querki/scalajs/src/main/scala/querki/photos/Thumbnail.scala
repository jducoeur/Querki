package querki.photos

import org.scalajs.dom
import org.scalajs.jquery._

import org.querki.facades.bootstrap._

import querki.globals._

import querki.display.input.InputGadget

class Thumbnail(implicit e:Ecology) extends InputGadget[dom.HTMLImageElement](e) {

  // At least for now, this just wraps incoming thumbnails:
  def doRender() = ???
  def values = ???
  
  def hook() = {
    println("Got a tooltip")
    $(elem).click({ evt:JQueryEventObject =>
      //showFull
    })
    
    $(elem).tooltip(TooltipOptions.title("Click to see full sized"))
  }
}
