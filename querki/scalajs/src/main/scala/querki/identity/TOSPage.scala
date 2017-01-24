package querki.identity

import scala.concurrent.Promise
import scala.util.Success

import scalatags.JsDom.all._
import rx._
import autowire._

import querki.display.{ButtonGadget, QText}
import querki.display.rx._
import querki.globals._
import querki.pages._
import querki.session.UserFunctions

import UserFunctions._

class TOSPage(onReady:Option[Unit => Unit])(implicit val ecology:Ecology) extends Page("tos")  {
  lazy val Client = interface[querki.client.Client]
  
  val agreed = Var[Boolean](false) 
  
  def pageContent = for {
    TOSInfo(version, text) <- Client[UserFunctions].fetchTOS().call()
    guts =
      div(
        h1(s"Querki Terms of Service, version $version"),
        p("Please read and agree to the following agreement in order to use Querki."),
        div(cls:="row",
          div(cls:="well col-md-offset1 col-md-10",
            new QText(text)
          ),
          
          div(cls:="col-md-12",
            p(new RxCheckbox(agreed, "I agree to the above Terms and Conditions.", id := "_TOSagree")),
            p(new ButtonGadget(ButtonGadget.Primary, "Submit", disabled := Rx { !agreed() } ) ({ () =>
              Client[UserFunctions].agreeToTOS(version).call().foreach { _ =>
                onReady.map(_(())).getOrElse(PageManager.reload())
              }
            }))
          )
        )
      )
  }
    yield PageContents(guts)
}

object TOSPage {
  /**
   * Make this Page composable.
   * 
   * TODO: this approach should probably be replaced by something better and more monadic.
   */
  def run(implicit ecology:Ecology):Future[Unit] = {
    val promise = Promise[Unit]
    val page = new TOSPage(Some(_ => promise.complete(Success(()))))
    ecology.api[querki.display.PageManager].renderPage(page)
    promise.future
  }
}
