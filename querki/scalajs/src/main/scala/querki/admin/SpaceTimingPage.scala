package querki.admin

import scalatags.JsDom.all._
import autowire._
import rx._

import querki.data.TOID
import querki.display._
import querki.display.rx._
import RxEmptyable._
import querki.globals._
import querki.pages._

/**
 * This is an edge-case Admin-only page. It's one of the better arguments for having a separate
 * Admin interface, since no way does this really need to be in user-downloaded code.
 */
class SpacesTimingPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
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
        yield p(a(href:="#", spaceId.underlying)),
      
      h3("Time a Space"),
      p("Enter just the pure OID (no dot) of the Space here:"),
      timeSpaceInput <= new RxText(id:="_timeSpaceInput"),
      new ButtonGadget(ButtonGadget.Primary, "Begin Timing", disabled := timeSpaceInput.rxEmpty)({ () =>
        timeSpaceInput.map { rxText =>
          Client[AdminFunctions].beginSpaceTiming(TOID(rxText.text())).call() map { _ =>
            // TODO: this should probably go directly to the SpaceTimingPage for this Space:
            PageManager.reload()
          }
        }
      })
    )
  }
    yield PageContents("Spaces Being Timed", guts)
}
