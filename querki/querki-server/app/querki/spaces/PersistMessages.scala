package querki.spaces

import org.joda.time.DateTime

import models.{OID}
import models.Kind.Kind
import models.MIMEType.MIMEType
import models.Thing.PropMap

import querki.values.SpaceState

/**
 * This object exists mainly for purposes of access and import control. The contained
 * case classes are what matter.
 */
private [spaces] object PersistMessages {
  
  // =====================================
  //
  // Space Persistence
  //
  
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
  
  case class SpaceChange(newName:String, newDisplay:String)
  /**
   * Command to alter the specified Thing. spaceChange should be given iff the Thing is the Space
   * itself. Should response with Changed().
   */
  case class Change(state:SpaceState, thingId:OID, modelId:OID, modTime:DateTime, props:PropMap, spaceChange:Option[SpaceChange])
  
  /**
   * Command to create a new Thing.
   */
  case class Create(state:SpaceState, modelId:OID, kind:Kind, props:PropMap, modTime:DateTime)
  
  /**
   * Response from a Change() or Create().
   */
  case class Changed(thingId:OID, timestamp:DateTime)
  
  // =====================================
  //
  // Space Manager Persistence
  //
  
  /**
   * Command to create a new Space. Note that ownerId should be an *Identity*.
   * 
   * userMaxSpaces is the maximum number of Spaces this owner is allowed to have; this will be
   * checked before creation.
   * 
   * name is the canonical name of the Space; display is the display name.
   * 
   * This will return either a Changed(), or ThingError.
   */
  case class CreateSpacePersist(ownerId:OID, userMaxSpaces:Int, name:String, display:String)
  
  /**
   * Command to fetch a Space by its path. OwnerId should be an Identity.
   * 
   * This will return either a SpaceId(), or ThingError.
   */
  case class GetSpaceByName(ownerId:OID, name:String)
  
  case class SpaceId(id:OID)
  
  /**
   * The general error response when things go wrong. This probably needs to become more
   * complex as we go along.
   */
  case object PersistError
}