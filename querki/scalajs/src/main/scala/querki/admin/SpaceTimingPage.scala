package querki.admin

import scala.concurrent.Future
import scala.scalajs.js
import js.timers._
import org.scalajs.dom.html

import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._
import org.querki.jquery._

import querki.data.TOID
import querki.display._
import querki.display.rx._
import QuerkiEmptyable._
import querki.globals._
import querki.pages._

import AdminFunctions._  

/**
 * This is an edge-case Admin-only page. It's one of the better arguments for having a separate
 * Admin interface, since no way does this really need to be in user-downloaded code.
 */
class SpacesTimingPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
  lazy val Admin = interface[Admin]
  lazy val Client = interface[querki.client.Client]
  
  val timeSpaceInput = GadgetRef[RxText]

  def pageContent = for {
    timedSpaces <- Client[AdminFunctions].getTimedSpaces().call()
    guts = div(
      h1("Spaces Being Timed"),
      p("""The following Spaces (if any) have monitoring turned on, to see how they are doing. This is a
          |fairly expensive process, and potentially an invasive one, so it *must* only be turned on for Spaces
          |that you have proper permission to access, preferably Querki-owned Spaces. It is a primitive
          |profiling tool, mainly intended for debugging.""".stripMargin),
      for (spaceId <- timedSpaces.toSeq)
        yield p(a(href:=Admin.spaceTimingFactory.pageUrl("spaceId" -> spaceId.underlying), spaceId.underlying)),
      
      h3("Time a Space"),
      p("Enter the OID (with the dot) of the Space here:"),
      timeSpaceInput <= new RxText(id:="_timeSpaceInput"),
      new ButtonGadget(ButtonGadget.Primary, "Begin Timing", disabled := timeSpaceInput.rxEmpty)({ () =>
        timeSpaceInput.map { rxText =>
          Client[AdminFunctions].beginSpaceTiming(TOID(rxText.text.now)).call() map { _ =>
            Admin.spaceTimingFactory.showPage("spaceId" -> rxText.text.now)
          }
        }
      })
    )
  }
    yield PageContents("Spaces Being Timed", guts)
}

class SpaceTimingPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
  lazy val Admin = interface[Admin]
  lazy val Client = interface[querki.client.Client]
  
  val spaceIdStr = params("spaceId")
  val spaceId = TOID(spaceIdStr)
  
  val messageOutput = GadgetRef.of[html.Div]
  
  val currentlyError:Var[Boolean] = Var(false)
  
  var _timeoutHandle:Option[SetTimeoutHandle] = None

  /**
   * This strobes the server once a second for new messages.
   */
  def updateMessages(index:Int):Unit = {
    val handle = setTimeout(1000) {
      Client[AdminFunctions].getSpaceTimingsSince(index, spaceId).call() foreach { case TimingMsgs(error, nowAt, newMsgs) =>
        if (error) {
          currentlyError() = true
        } else {
          currentlyError() = false
          newMsgs foreach { msg =>
            val para = p(msg).render
            messageOutput.mapElem($(_).append(para))
          }
        }
        updateMessages(nowAt)
      }
    }
    _timeoutHandle = Some(handle)
  }

  def pageContent = for {
    _ <- Future(updateMessages(0))
    guts = div(
      h1(s"Timings for Space $spaceIdStr"),
      
      div(cls:="alert alert-warning", "Currently timing out", display := Rx{ if (currentlyError()) "block" else "none" }),
      
      new ButtonGadget(ButtonGadget.Warning, s"Stop timing $spaceIdStr")({() =>
        _timeoutHandle.map(clearTimeout(_))
        _timeoutHandle = None
        Client[AdminFunctions].stopSpaceTiming(spaceId).call() foreach { _ =>
          Admin.spacesTimingFactory.showPage()
        }
      }),
      
      // This is where the messages will actually go:
      messageOutput <= div()
    )
  }
    yield PageContents(s"Timings for Space $spaceIdStr", guts)
}
