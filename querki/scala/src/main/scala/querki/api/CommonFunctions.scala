package querki.api

trait CommonFunctions {
  /**
   * Fetches the "standard" info, which the Client needs to work with the Server.
   */
  def getStandardInfo():StandardInfo
}

/**
 * The "standard" info about the Server. This is a grab-bag of information that the Client fetches.
 */
case class StandardInfo(
  detailsPropId:String,
  summaryPropId:String,
  urPropId:String,
  namePropId:String,
  collPropId:String,
  typePropId:String
)
