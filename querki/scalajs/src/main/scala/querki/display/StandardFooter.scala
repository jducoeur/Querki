package querki.display

import org.scalajs.dom.{raw => dom}

import scalatags.JsDom.all._
import org.querki.gadgets._

import querki.globals._

import querki.comm._

class StandardFooter(implicit val ecology: Ecology) extends Gadget[dom.HTMLElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Identity = interface[querki.identity.Identity]

  def doRender() =
    footer(
      cls := "_mainFooter _noPrint",
      hr,
      b("Notice: "),
      "Querki will be occasionally down for maintenance during February and March 2025, to modernize our systems. Please bear with us.",
      hr,
      "Querki ",
      raw("&copy;"),
      " Querki Inc 2013-2025 | ",
      a(href := controllers.ClientController.space("systemUser", "documentation").url, "Help", tabindex := 100000),
      " | ",
      a(href := Identity.tosFactory.pageUrl(), "Terms of Service", tabindex := 100010),
      " | ",
      "v",
      DataAccess.querkiVersion
    )
}
