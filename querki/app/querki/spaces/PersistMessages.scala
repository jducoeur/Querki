package querki.spaces

import models.{OID}

import querki.values.SpaceState

/**
 * This object exists mainly for purposes of access and import control. The contained
 * case classes are what matter.
 */
private [spaces] object PersistMessages {
  /**
   * Request from the Space to the Persister, send when the Space is booted. Persister should
   * respond with Loaded.
   */
  case object Load
  
  /**
   * Response sent when the Persister successfully has loaded the Space.
   */
  case class Loaded(state:SpaceState)

  /**
   * Command to delete the Thing with the specified OID. Fire-and-forget, with no response.
   */
  case class Delete(thingId:OID)
  
  /**
   * The general error response when things go wrong. This probably needs to become more
   * complex as we go along.
   */
  case object PersistError
}