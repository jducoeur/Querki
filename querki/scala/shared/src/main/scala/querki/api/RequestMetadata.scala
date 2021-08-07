package querki.api

import querki.history.HistoryFunctions.HistoryVersion

/**
 * Additional information that goes with *all* API requests.
 *
 * @TBD: I can argue that asOf should be done as a cookie instead. The question is mainly how
 * persistent we want it to be.
 *
 * @param version The version of the Client that we are currently using.
 * @param pageParams A map of the URL parameters, which may be used by QL under some circumstances.
 */
case class RequestMetadata(
  version: String,
  pageParams: Map[String, String]
)
