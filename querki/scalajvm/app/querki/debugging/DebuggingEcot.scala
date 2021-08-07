package querki.debugging

import scala.concurrent.duration._
import akka.pattern._
import akka.util.Timeout
import querki.console.CommandEffectArgs
import querki.console.ConsoleFunctions.{DisplayTextResult, ErrorResult}
import querki.ecology.{EcotIds, QuerkiEcot}
import querki.globals._
import querki.session.messages._
import querki.spaces.messages.SpaceSubsystemRequest

object MOIDs extends EcotIds(77) {
  val DebugLogMethodOID = moid(1)
  val GetDebugLogCommandOID = moid(2)
  val ClearDebugLogCommandOID = moid(3)
}

class DebuggingEcot(e: Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._

  val AccessControl = initRequires[querki.security.AccessControl]
  val Console = initRequires[querki.console.Console]

  lazy val SpaceOps = interface[querki.spaces.SpaceOps]

  /**
   * *********************************************
   * FUNCTIONS
   * *********************************************
   */

  lazy val DebugLogMethod = new InternalMethod(
    DebugLogMethodOID,
    toProps(
      setName("_debugLog"),
      Categories(DebuggingTag),
      Summary("Sends the given value to the Debug Log"),
      Details(
        """This provides a very primitive but useful debugging tool. This function receives any
          |value you like, and should have exactly one parameter. That parameter is rendered in the context
          |of the received value, and that rendering is placed in the Debug Log. It then produces the received
          |value.
          |
          |This means that you can stick `_debugLog` into the middle of any QL phrase when necessary; it will
          |log as directed, and then keep going.
          |
          |To see the Debug Log, go into the Querki Console and give the `Show Debug Log` command. (In the long
          |run, we plan to automatically push these logs to the Console, to make this easier to use.)
          |
          |Note that the Debug Log can show up slightly out of order, if events happen very close together.
          |This is inherent in the way Querki works; don't be too surprised by it.
          |
          |This only works for logged-in users, by the nature of how this command works. If the current viewer
          |isn't logged in, this will just silently pass through and not do anything.
        """.stripMargin
      ),
      Signature(
        expected = Some(Seq(AnyType), "Anything"),
        reqs = Seq(
          ("value", AnyType, "The value that will be rendered into the Debug Log")
        ),
        opts = Seq.empty,
        returns = (AnyType, "The value that was originally received")
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      val isRealUser = inv.context.request.requester.isDefined
      if (isRealUser) {
        println(s"Recording the Debug Log")
        val result: QFut = for {
          param <- inv.process("value")
          rendered <- inv.fut(param.wikify(inv.context))
          requester = inv.context.request.requesterOrAnon
          request = SpaceSubsystemRequest(requester, inv.state.id, AddToDebugLog(rendered))
          // Note that this one is intentionally fire-and-forget:
          _ = SpaceOps.spaceRegion ! request
        } yield inv.context.value
        result
      } else {
        // Only logged-in users will show up here:
        fut(inv.context.value)
      }
    }
  }

  /**
   * *********************************************
   * COMMANDS
   * *********************************************
   */

  lazy val GetDebugLogCommand = Console.defineSpaceCommand(
    GetDebugLogCommandOID,
    "Show Debug Log",
    "Displays everything in your current Debug Log",
    // This is low-security, because all it shows is what you have done yourself:
    Seq(AccessControl.CanReadProp)
  ) {
    case CommandEffectArgs(inv, api) => {
      inv.context.request.requester match {
        case Some(requester) => {
          val request = SpaceSubsystemRequest(requester, inv.state.id, GetDebugLog())
          implicit val timeout = Timeout(2 seconds)
          (SpaceOps.spaceRegion ? request).map {
            case CurrentDebugLog(msgs) => {
              if (msgs.isEmpty) {
                DisplayTextResult("The Debug Log is empty.")
              } else {
                // Note that the messages are current in reverse order
                val text = msgs.map(_.strip.toString()).reverse.mkString("\n")
                DisplayTextResult(text)
              }
            }
            case _ => ErrorResult("Got an expected result!")
          }
        }
        case None => fut(ErrorResult("The `Show Debug Log` command is only available when you are logged in."))
      }
    }
  }

  lazy val ClearDebugLogCommand = Console.defineSpaceCommand(
    ClearDebugLogCommandOID,
    "Clear Debug Log",
    "Clears Debug Log commands so far",
    Seq(AccessControl.CanReadProp)
  ) {
    case CommandEffectArgs(inv, api) => {
      inv.context.request.requester match {
        case Some(requester) => {
          val request = SpaceSubsystemRequest(requester, inv.state.id, ClearDebugLog())
          implicit val timeout = Timeout(2 seconds)
          (SpaceOps.spaceRegion ? request).map {
            case DebugLogCleared => DisplayTextResult("Debug log cleared.")
            case _               => ErrorResult("Got an expected result!")
          }
        }
        case None => fut(ErrorResult("The `Show Debug Log` command is only available when you are logged in."))
      }
    }
  }

  override lazy val props = Seq(
    DebugLogMethod,
    GetDebugLogCommand,
    ClearDebugLogCommand
  )
}
