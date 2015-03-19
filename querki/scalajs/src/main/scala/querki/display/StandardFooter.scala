package querki.display

import org.scalajs.dom.{raw => dom}

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._

class StandardFooter(implicit val ecology:Ecology) extends Gadget[dom.HTMLElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  def doRender() =
    footer(cls:="_mainFooter _noPrint",
      hr,
      "Querki ", raw("&copy;"), " Querki Inc 2013-2015 | ",
      a(href:=controllers.ClientController.space("systemUser", "documentation").url, "Help"),
      " | ",
      a(href:=controllers.TOSController.showTOS().url, "Terms of Service")
    )
}
