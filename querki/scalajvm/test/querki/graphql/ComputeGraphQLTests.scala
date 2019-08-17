package querki.graphql

import cats.effect._
import cats.implicits._
import org.scalactic.source.Position
import play.api.libs.json._
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

  implicit class RichJsValue(jsv: JsValue) {
    def field(path: String)(implicit p: Position): JsValue =
      (jsv \ path).getOrElse(fail(s"Couldn't find path $path in object $jsv"))

    def obj(path: String)(implicit p: Position): JsObject = {
      field(path) match {
        case o: JsObject => o
        case other => fail(s"Field $path wasn't an Object: $other")
      }
    }

    def string(path: String)(implicit p: Position): String = {
      field(path) match {
        case JsString(s) => s
        case other => fail(s"Field $path wasn't a String: $other")
      }
    }

    def array(path: String)(implicit p: Position): Seq[JsValue] = {
      field(path) match {
        case JsArray(a) => a
        case other => fail(s"Field $path wasn't an Array: $other")
      }
    }

    def name: String = string("_name")
    def hasName(n: String) = name == n
  }

  implicit class RichJsSequence(seq: Seq[JsValue]) {
    def findByName(name: String)(implicit p: Position): JsValue = {
      seq.find(_.hasName(name)).getOrElse(fail(s"Couldn't find an element named $name in $seq"))
    }
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
        val thing = data.obj("_thing")
        thing.string("_oid") shouldBe (eurythmicsOID.toThingId.toString)
        thing.string("_name") shouldBe (cdSpace.eurythmics.linkName.get)
      }
    }

    "handle top-level aliases" in {
      implicit val cdSpace = new CDSpace
      val eurythmicsOID = cdSpace.eurythmics.id

      runQueryAndCheckData(
        s"""
           |query CDQuery {
           |  justEurythmics: _thing(_oid: "$eurythmicsOID") {
           |    _oid
           |    _name
           |  }
           |}
        """.stripMargin
      ) { data =>
        val thing = data.obj("justEurythmics")
        thing.string("_oid") shouldBe (eurythmicsOID.toThingId.toString)
        thing.string("_name") shouldBe (cdSpace.eurythmics.linkName.get)
      }
    }

    // Also tests:
    // * Getting the initial object by name instead of OID
    // * Working with all the Instances of a Model
    // * Fetching Display Names
    // * Getting Tags as names
    "dereference and drill down into Links in Instances" in {
      implicit val cdSpace = new CDSpace

      runQueryAndCheckData(
        s"""
           |query AlbumQuery {
           |  artistsForAlbums: _instances(_name: "Album") {
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
      ) { data =>
        val albums = data.array("artistsForAlbums")

        // Spot-check some elements in here:
        locally {
          val album = albums.findByName("Be-Yourself-Tonight")
          val artist = album.array("Artists").findByName("Eurythmics")
          val genres = artist.array("Genres")
          genres.length shouldBe (1)
          genres.head shouldBe (JsString("Rock"))
        }

        locally {
          val album = albums.findByName("Classical-Randomness")
          album.array("Artists") shouldBe empty
        }

        locally {
          val album = albums.findByName("Flood")
          val artist = album.array("Artists").find(_.string("Name") == "They Might Be Giants").get
          val genres = artist.array("Genres")
          genres.length shouldBe (2)
          genres should contain (JsString("Rock"))
          genres should contain (JsString("Weird"))
        }
      }
    }

    "dereference and drill into Tags when requested" in {
      implicit val cdSpace = new CDSpace

      runQueryAndCheckData(
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
      ) { data =>
        val exemplarName = data
          .obj("_thing")
          .array("Genres")
          .head
          .obj("Exemplar")
          .name
        exemplarName shouldBe ("Blackmores-Night")
      }
    }
  }
}

// TODO: rendering of text fields, support from Console, and real plumbing