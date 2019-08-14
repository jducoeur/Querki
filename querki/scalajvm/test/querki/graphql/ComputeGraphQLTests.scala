package querki.graphql

import cats.effect._
import cats.implicits._
import play.api.libs.json.Json
import querki.test.{CDSpace, QuerkiTests}
import querki.util.QLog

class ComputeGraphQLTests extends QuerkiTests {
  "FPComputeGraphQL" should {
    "process a basic query" in {
      val cdSpace = new CDSpace

      val computer = new FPComputeGraphQL()(cdSpace.state, ecology)

      val eurythmicsOID = cdSpace.eurythmics.id
      val thingQuery =
        s"""
          |query CDQuery {
          |  _thing(_oid: "$eurythmicsOID") {
          |    _oid
          |    _name
          |  }
          |}
        """.stripMargin
      val thingJsv = computer.handle(thingQuery).unsafeRunSync()
      println(Json.prettyPrint(thingJsv))

      val instancesQuery =
        s"""
           |query AlbumQuery {
           |  _instances(_name: "Album") {
           |    _oid
           |    _name
           |    Artists {
           |      _name
           |      Name
           |    }
           |  }
           |}
         """.stripMargin
      val instancesJsv = computer.handle(instancesQuery).unsafeRunSync()
      println(Json.prettyPrint(instancesJsv))
    }
  }
}
