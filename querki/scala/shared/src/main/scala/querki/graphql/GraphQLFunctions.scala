package querki.graphql

import scala.concurrent.Future

trait GraphQLFunctions {

  /**
   * Run the given GraphQL expression, and return the result.
   *
   * Note that the result is a JSON String, which will contain errors if there are any. This should never
   * return a failed Future.
   *
   * @param graphQL a GraphQL expression that is legal for Querki
   * @param pretty iff true, the resulting String will be pretty-printed
   * @return the resulting JSON structure, rendered as a String
   */
  def runGraphQL(
    graphQL: String,
    pretty: Boolean = false
  ): Future[String]
}
