package querki.display.input

import scala.scalajs.js
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import querki.globals._

class CheckboxGadget(implicit e: Ecology) extends InputGadget[dom.HTMLInputElement](e) {

  def values = {
    if ($(elem).prop("checked").asInstanceOf[Boolean])
      List("on")
    else
      List("off")
  }

  def hook() = {
    $(elem).change({ e: dom.Element =>
      save()
    })
  }

  def doRender() = ???
}
