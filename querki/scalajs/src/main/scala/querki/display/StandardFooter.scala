package querki.display

import org.scalajs.dom.{raw => dom}

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._

class StandardFooter(implicit val ecology:Ecology) extends Gadget[dom.HTMLElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  def doRender() =
    footer(cls:="_mainFooter _noPrint",
      hr,
      "Querki ", raw("&copy;"), " Querki Inc 2013-2017 | ",
      a(href:=controllers.ClientController.space("systemUser", "documentation").url, "Help", tabindex:=100000),
      " | ",
      a(href:=controllers.TOSController.showTOS().url, "Terms of Service", tabindex:=100010),
      " | ",
      "v", DataAccess.querkiVersion
    )
}
