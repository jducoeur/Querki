package querki.api

/**
 * Additional information that goes with *all* API requests.
 * 
 * @param pageParams A map of the URL parameters, which may be used by QL under some circumstances.
 */
case class RequestMetadata(version:String, pageParams:Map[String,String])
