package querki.email

import org.scalajs.dom.html
import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._

import models.Wikitext
import querki.data.TOID
import querki.display._
import querki.globals._
import querki.pages._

import EmailFunctions._

class UnsubscribePage(params: ParamMap)(implicit val ecology: Ecology) extends Page("unsubscribe") {

  lazy val Client = interface[querki.client.Client]

  // This page is only valid if the server provided a payload. This is an opaque value that we
  // page back to the server to set up the page:
  lazy val unsubInfoStr = DataAccess.request.payloadOpt.get

  class UnsubRow(
    opt: UnsubOption,
    identityId: TOID
  ) extends Gadget[html.TableRow] {
    val resultMsg = Var(Wikitext(""))

    def doRender() = {
      tr(
        cls := "_unsubItem",
        id := opt.unsubId.underlying,
        td(
          new ButtonGadget(ButtonGadget.Normal, opt.label)({ () =>
            Client[EmailFunctions].unsubscribe(opt.notifier, identityId, opt.unsubId, opt.context).call().foreach {
              message =>
                resultMsg() = message
            }
          })
        ),
        td(
          QText(opt.desc),
          // This gets reactively filled with the response message if the user presses the button:
          new RxDiv(Rx { Seq(new QText(resultMsg())) })
        )
      )
    }
  }

  def pageContent = for {
    unsubPageInfo <- Client[EmailFunctions].getUnsubOptionsFor(unsubInfoStr).call()
    guts = div(
      h1("Unsubscribe"),
      new QText(unsubPageInfo.emailInfo),
      table(
        cls := "table table-hover",
        tbody(
          for (opt <- unsubPageInfo.options)
            yield new UnsubRow(opt, unsubPageInfo.identityId)
        )
      )
    )
  } yield PageContents(guts)
}
