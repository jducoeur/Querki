package querki.graphql

import cats.effect._
import cats.implicits._
import org.scalactic.source.Position
import play.api.libs.json.{Json, JsObject, JsValue, JsString}
import querki.test.{TestSpace, CDSpace, QuerkiTests}
import querki.util.QLog

class ComputeGraphQLTests extends QuerkiTests {
  def runQueryAndCheck[S <: TestSpace](query: String)(check: JsValue => Unit)(implicit space: S, p: Position): Unit = {
    val computer = new FPComputeGraphQL()(space.state, ecology)
    val jsv = computer.handle(query).unsafeRunSync()
    check(jsv)
  }

  def runQueryAndCheckData[S <: TestSpace](query: String)(check: JsObject => Unit)(implicit space: S, p: Position): Unit = {
    runQueryAndCheck(query) { jsv =>
      val data = (jsv \ "data").getOrElse(fail(s"Query result didn't have a data field: $jsv"))
      data match {
        case jso: JsObject => check(jso)
        case other => fail(s"Resulting data field wasn't a JsObject: $other")
      }
    }
  }

  def runQueryAndPrettyPrint[S <: TestSpace](query: String)(implicit space: S, p: Position): Unit = {
    runQueryAndCheck(query)(jsv => println(Json.prettyPrint(jsv)))
  }

  "FPComputeGraphQL" should {
    "process a basic query" in {
      implicit val cdSpace = new CDSpace
      val eurythmicsOID = cdSpace.eurythmics.id

      runQueryAndCheckData(
        s"""
          |query CDQuery {
          |  _thing(_oid: "$eurythmicsOID") {
          |    _oid
          |    _name
          |  }
          |}
        """.stripMargin
      ) { data =>
        (data \ "_thing" \ "_oid").get shouldBe (JsString(eurythmicsOID.toThingId.toString))
        (data \ "_thing" \ "_name").get shouldBe (JsString(cdSpace.eurythmics.linkName.get))
      }
//      val thingQuery =
//        s"""
//          |query CDQuery {
//          |  justEurythmics: _thing(_oid: "$eurythmicsOID") {
//          |    _oid
//          |    _name
//          |  }
//          |}
//        """.stripMargin
//      runQuery(thingQuery)
//
//      val instancesQuery =
//        s"""
//           |query AlbumQuery {
//           |  artistsForAlbums: _instances(_name: "Album") {
//           |    _oid
//           |    _name
//           |    Artists {
//           |      _name
//           |      Name
//           |      Genres
//           |    }
//           |  }
//           |}
//         """.stripMargin
//      runQuery(instancesQuery)
//
//      val drillDownTagsQuery =
//        """
//          |query DrillDownTagQuery {
//          |  _thing(_name: "Gordon-Bok") {
//          |    Genres {
//          |      Exemplar {
//          |        _name
//          |      }
//          |    }
//          |  }
//          |}
//        """.stripMargin
//      runQuery(drillDownTagsQuery)
    }
  }
}

// TODO: rendering of text fields, unit tests, support from Console, and real plumbing