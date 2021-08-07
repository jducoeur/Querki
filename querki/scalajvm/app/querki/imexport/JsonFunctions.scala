package querki.imexport

import scala.concurrent.Future

import querki.data.TID

/**
 * This is an Autowire API. It isn't defined in shared because we currently aren't using it from there --
 * we're just using it from the JsonController to the back end. If we ever want it from the client, move
 * this declaration to shared.
 */
trait JsonFunctions {

  /**
   * Produces the JSON for the specified Thing and Property.
   */
  def getJsonFor(
    thingId: TID,
    propId: Option[TID]
  ): Future[String]
}
