package querki.email

import scalatags.JsDom.all._
import autowire._

import querki.display._
import querki.globals._
import querki.pages._

import EmailFunctions._

class UnsubscribePage(params:ParamMap)(implicit val ecology:Ecology) extends Page("unsubscribe") {
  
  lazy val Client = interface[querki.client.Client]
  
  // This page is only valid if the server provided a payload. This is an opaque value that we
  // page back to the server to set up the page:
  lazy val unsubInfoStr = DataAccess.request.payloadOpt.get
  
  def pageContent = for {
    unsubPageInfo <- Client[EmailFunctions].getUnsubOptionsFor(unsubInfoStr).call()
    guts = div(
      h1("Unsubscribe"),
      new QText(unsubPageInfo.emailInfo),
      table(cls:="table table-hover",
        tbody(
          for (opt <- unsubPageInfo.options)
            yield Gadget(
              tr(cls:="_unsubItem",
                id:=opt.id.underlying,
                td(
                  new ButtonGadget(ButtonGadget.Normal, opt.label)({() =>
                    // TODO: Actually do the Unsub
                  })
                ),
                td(
                  QText(opt.desc)
                )
              )
            )
        )
      )
    )
  }
    yield PageContents(guts)
}
