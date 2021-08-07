package querki.display

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import org.querki.gadgets._

import querki.globals._

class StatusTextGadget(implicit val ecology: Ecology) extends Gadget[dom.HTMLSpanElement] {
  def doRender() = span(id := "statusText")
}

class StatusLineGadget(implicit val ecology: Ecology) extends Gadget[dom.HTMLAnchorElement] {
  lazy val textGadget = new StatusTextGadget

  def doRender() =
    a(id := "statusLine", href := "#", textGadget)
}

private[display] trait StatusLineInternal extends EcologyInterface {
  def statusGadget: StatusLineGadget
}

class StatusLineEcot(e: Ecology) extends ClientEcot(e) with StatusLine with StatusLineInternal {
  def implements = Set(classOf[StatusLine], classOf[StatusLineInternal])

  lazy val statusGadget = new StatusLineGadget

  def showInternal(
    msg: String,
    showCmd: JQuery => Unit
  ) = {
    statusGadget.textGadget.elemOpt match {
      case Some(elem) => {
        $(elem).text(msg)
        showCmd($(elem))
      }
      case None =>
    }
  }

  def showUntilChange(msg: String): Unit = showInternal(msg, { jq => jq.show() })

  def showBriefly(msg: String): Unit = showInternal(msg, { jq => jq.show().delay(4000).hide("slow") })

  def clear(): Unit = showInternal("", { jq => jq.hide("slow") })
}
