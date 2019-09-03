package querki.graphql

import java.nio.charset.StandardCharsets

import org.scalatest.Matchers._
import querki.test.mid._
import AllFuncs._
import controllers.GraphQLController
import org.scalatest.tags.Slow
import play.api.libs.json._
import play.api.test._
import play.api.test.Helpers._
import querki.console.ConsoleMidFuncs._

object GraphQLMidTests {
  // This is just testing the Run GraphQL command in the Console; actually testing GraphQL features happens in
  // ComputeGraphQL tests. So we set up an extremely simple Space for testing:
  lazy val basicGraphQLTest: TestOp[Unit] = {
    for {
      _ <- step("GraphQL smoketest suite")
      std <- getStd
      basicUser = TestUser("GraphQL Test User")
      basicSpaceName = "GraphQL Test Space"

      // Set up the Space:
      loginResults <- newUser(basicUser)
      mainSpace <- createSpace(basicSpaceName)
      propId <- makeProperty("First Property", exactlyOne, textType)
      modelId <- makeModel("First Model", propId :=> "")
      instanceId <- makeThing(modelId, "First Instance", propId :=> "Instance value")

      // Test it directly:
      graphQL =
        """
          |query SimpleGraphQLTest {
          |  _instances(_name: "First-Model") {
          |    _oid
          |    _name
          |    First_Property
          |  }
          |}
        """.stripMargin
      cmd = s"""Run GraphQL(""$graphQL"")"""
      result <- consoleCommandWithTextResult(cmd)
      json = Json.parse(result)
      instance = ((json \\ "data").head \ "_instances").head
      _ = (instance \ "_name").as[String] shouldBe "First-Instance"
      _ = (instance \ "First_Property").as[String] shouldBe "Instance value"

      // Test the plumbing:
      request =
        FakeRequest("POST", s"/graphql/${basicUser.handle}/${mainSpace.underlying.toString}")
          .withHeaders("Content-Type" -> "application/graphql")
          .withBody(graphQL)
      responseBody <- TestOp.fut { state =>
        import state.harness._
        for {
          result <- call(controller[GraphQLController].graphQL(basicUser.handle, mainSpace.underlying.toString), request)
          byteString <- result.body.consumeData
          json = byteString.decodeString(StandardCharsets.UTF_8)
        }
          yield (state, json)
      }
      plumbingJson = Json.parse(responseBody)
      plumbingInstance = ((plumbingJson \\ "data").head \ "_instances").head
      _ = (plumbingInstance \ "_name").as[String] shouldBe "First-Instance"
      _ = (plumbingInstance \ "First_Property").as[String] shouldBe "Instance value"
    }
      yield ()
  }
}

@Slow
class GraphQLMidTests extends MidTestBase {
  import GraphQLMidTests._

  "The Run GraphQL command" should {
    "work with some simple GraphQL" in {
      runTest(basicGraphQLTest)
    }
  }
}
