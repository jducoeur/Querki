package querki.uservalues

import models.OID

import querki.identity.PublicIdentity
import querki.time.DateTime
import querki.values.{QValue, SpaceState}

object PersistMessages {
  case class OneUserValue(identity:PublicIdentity, thingId:OID, propId:OID, v:QValue, modTime:DateTime)
  
  /**
   * Pulls in all of the User Values for the specified User. (Note that the Space is already implicit
   * in this Persister.)
   */
  case class LoadValuesForUser(identity:PublicIdentity, state:SpaceState)
  
  /**
   * Results of LoadValuesForUser.
   */
  case class ValuesForUser(values:Seq[OneUserValue])
  
  /**
   * Saves the specified User Value.
   */
  case class SaveUserValue(uv:OneUserValue, state:SpaceState, update:Boolean)
  
  /**
   * Marker trait for the messages we are accepting from static, non-Space code.
   */
  sealed trait ExternallyExposed
  
  /**
   * Loads all of the User Values for the specified Thing/Property. Returns a ValuesForUser.
   */
  case class LoadThingPropValues(thingId:OID, propId:OID, state:SpaceState) extends ExternallyExposed
}