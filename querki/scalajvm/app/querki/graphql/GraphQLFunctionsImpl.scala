package querki.graphql

import play.api.libs.json.Json
import querki.globals._
import querki.api.{AutowireParams, SpaceApiImpl}

import scala.concurrent.Future

class GraphQLFunctionsImpl(info: AutowireParams)(implicit e: Ecology)
  extends SpaceApiImpl(info, e)
     with GraphQLFunctions {

  def doRoute(req: Request): Future[String] = route[GraphQLFunctions](this)(req)

  def runGraphQL(
    query: String,
    pretty: Boolean = false
  ): Future[String] = {
    val computer = new FPComputeGraphQL()(rc, state, ecology)
    val built = computer.handle(query)
    built.unsafeToFuture().map { json =>
      if (pretty)
        Json.prettyPrint(json)
      else
        Json.stringify(json)
    }
  }
}
