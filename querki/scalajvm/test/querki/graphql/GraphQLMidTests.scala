package querki.graphql

import org.scalatest.Matchers._
import querki.test.mid._
import AllFuncs._
import org.scalatest.tags.Slow
import play.api.libs.json._
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

      loginResults <- newUser(basicUser)
      mainSpace <- createSpace(basicSpaceName)
      propId <- makeProperty("First Property", exactlyOne, textType)
      modelId <- makeModel("First Model", propId :=> "")
      instanceId <- makeThing(modelId, "First Instance", propId :=> "Instance value")

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
