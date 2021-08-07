package controllers

import java.nio.charset.StandardCharsets

import autowire._
import javax.inject.{Inject, Provider}
import querki.globals._
import querki.graphql.GraphQLFunctions

import scala.concurrent.Future

class GraphQLController @Inject() (val appProv: Provider[play.api.Application]) extends ApplicationBase {

  def graphQL(
    ownerId: String,
    spaceIdStr: String
  ) = withLocalClient(ownerId, spaceIdStr) { (rc, client) =>
    val resultOpt = for {
      rawBuffer <- rc.request.body.asRaw
      bytes <- rawBuffer.asBytes()
      query = bytes.decodeString(StandardCharsets.UTF_8)
    } yield client[GraphQLFunctions].runGraphQL(query).call()

    resultOpt.map { resultFut =>
      resultFut.map(Ok(_))
    }.getOrElse(
      Future.successful(BadRequest(
        "The content-type should be 'application/graphql', and there must be UTF-8 encoded GraphQL in the body"
      ))
    )
  }
}
