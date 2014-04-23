package querki.uservalues

import models.OID

import querki.time.DateTime
import querki.values.{QValue, SpaceState}

object PersistMessages {
  case class OneUserValue(thingId:OID, propId:OID, v:QValue, modTime:DateTime)
  
  /**
   * Pulls in all of the User Values for the specified User. (Note that the Space is already implicit
   * in this Persister.)
   */
  case class LoadValuesForUser(identityId:OID, state:SpaceState)
  
  /**
   * Results of LoadValuesForUser.
   */
  case class ValuesForUser(identityId:OID, values:Seq[OneUserValue])
  
  /**
   * Saves the specified User Value.
   */
  case class SaveUserValue(identityId:OID, uv:OneUserValue, state:SpaceState, update:Boolean)
}