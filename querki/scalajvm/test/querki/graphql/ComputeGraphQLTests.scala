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

      def runQuery(query: String): Unit = {
        val jsv = computer.handle(query).unsafeRunSync()
        println(Json.prettyPrint(jsv))
      }

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
      runQuery(thingQuery)

      val instancesQuery =
        s"""
           |query AlbumQuery {
           |  _instances(_name: "Album") {
           |    _oid
           |    _name
           |    Artists {
           |      _name
           |      Name
           |      Genres
           |    }
           |  }
           |}
         """.stripMargin
      runQuery(instancesQuery)

      val drillDownTagsQuery =
        """
          |query DrillDownTagQuery {
          |  _thing(_name: "Gordon-Bok") {
          |    Genres {
          |      Exemplar {
          |        _name
          |      }
          |    }
          |  }
          |}
        """.stripMargin
      runQuery(drillDownTagsQuery)
    }
  }
}
