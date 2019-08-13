package querki.graphql

import sangria.macros._
import querki.test.QuerkiTests
import sangria.parser.QueryParser

class OldComputeGraphQLTests extends QuerkiTests {
  "computeGraphQL" should {
//    "produce a reasonable schema" in {
//      implicit val s = commonSpace
//
//      val schema = ComputeGraphQL.computeGraphQLSchema(s.state)
//      println(schema)
//    }

    "parse a query" in {
      val query = """query HeroAndFriends {
                    |  hero {
                    |    name
                    |    friends {
                    |      name
                    |    }
                    |  }
                    |}""".stripMargin

      val result = QueryParser.parse(query)
      println(result)
    }
  }
}
