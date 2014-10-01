package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.comm._

class StandardFooter extends Gadget[dom.HTMLElement] {
  def doRender() =
    footer(cls:="_mainFooter _noPrint",
      hr,
      raw("&copy;"), " Querki 2013-2014 | ",
      a(href:=controllers.Application.thing("systemUser", "documentation", "documentation").url, "Help"),
      " | ",
      a(href:=controllers.TOSController.showTOS().url, "Terms of Service")
    )
}
