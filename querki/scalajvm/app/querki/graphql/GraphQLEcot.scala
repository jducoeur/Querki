package querki.graphql

import play.api.libs.json.Json
import querki.console.CommandEffectArgs
import querki.console.ConsoleFunctions.{ErrorResult, DisplayTextResult}
import querki.ecology.{QuerkiEcot, EcotIds}
import querki.globals._
import querki.ql.{QLExp, QLTextStage, UnQLText}

object MOIDs extends EcotIds(76) {
  val GraphQLCommandOID = moid(1)
}

class GraphQLEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._

  val AccessControl = initRequires[querki.security.AccessControl]
  val Basic = initRequires[querki.basic.Basic]
  val Console = initRequires[querki.console.Console]

  lazy val GraphQLCommand = Console.defineSpaceCommand(
    GraphQLCommandOID,
    "Run GraphQL",
    "Processes the given GraphQL query and shows the result",
    // This is intentionally very low-security, since it shouldn't be able to do anything you can't do anyway:
    Seq(AccessControl.CanReadProp)
  ) {
    case CommandEffectArgs(inv, api) =>
      implicit val state = inv.state

      // AFAIK we don't have a "raw text" type yet, and letting the query get processed as PlainText or QLText
      // winds up mutating it. So extract the raw String.
      // TODO: introduce or find a proper internal Raw String type, for situations like this where I want
      // exactly the contents of the parameter, with zero processing:
      def extractQuery(qlExp: QLExp): Option[String] = {
        for {
          phrase <- qlExp.phrases.headOption
          stage <- phrase.ops.headOption
          textStage <- stage match {
            case ts: QLTextStage => Some(ts)
            case _ => None
          }
        }
          yield textStage.contents.reconstructString
      }

      val result = for {
        paramOpt <- inv.rawParam(0)
        queryOpt = extractQuery(paramOpt)
        if (queryOpt.isDefined)
        query = queryOpt.get
        computer = new FPComputeGraphQL()(inv.context.request, inv.state, ecology)
        built = computer.handle(query)
        jsv <- inv.fut(built.unsafeToFuture())
        out = Json.prettyPrint(jsv)
      }
        yield DisplayTextResult(out)

      result.get.map(_.headOption.getOrElse(ErrorResult(s"You need to specify the GraphQL as a parameter")))
  }

  override lazy val props = Seq(
    GraphQLCommand
  )
}
