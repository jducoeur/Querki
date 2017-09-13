package querki.history

import scala.concurrent.duration._
import akka.pattern._
import akka.util.Timeout

import querki.console.ConsoleFunctions._
import querki.globals._

class HistoryCommands(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._
  import SpaceHistory._
  
  val Console = initRequires[querki.console.Console]
  val History = initRequires[History]
  
  implicit val timeout = Timeout(30 seconds)
  
  lazy val FindAllStompedCmd = Console.defineSpaceCommand(
    FindAllStompedCmdOID, 
    "Find All Stomped",
    """This displays a list of all Things that were "stomped" by the duplicate-OID bug.""",
    Seq(History.HistoryPerm))
  { args =>
    
    val inv = args.inv 
    implicit val state = inv.state
    // TODO: this is icky, even though I believe it's always true. Really, all of this is proving
    // that the whole bloody ApiImpl thing should have been done in terms of typeclasses instead
    // of subclasses. *Sigh*.
    val spaceApi = args.api.asInstanceOf[querki.api.SpaceApiImpl]
    
    for {
      StompedThings(things) <- spaceApi.spaceRouter ? FindAllStomped()
    }
      yield DisplayTextResult(
        s"""Stomped items found in Space ${state.displayName}:
           |${things.map(item => s"  ${item.event}: ${item.display} (${item.oid})").mkString("\n")}""".stripMargin)
  }
  
  override lazy val props = Seq(
    FindAllStompedCmd
  )
}
