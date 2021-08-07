package querki.spaces

import org.joda.time.DateTime

import models._
import models.Kind.Kind
import models.MIMEType.MIMEType

import querki.identity.User
import querki.values.SpaceState

/**
 * This object exists mainly for purposes of access and import control. The contained
 * case classes are what matter.
 */
private[spaces] object PersistMessages {

  // =====================================
  //
  // Space Persistence
  //

  /**
   * Request from the Space to the Persister, to do any necessary pre-load evolution of the Space's
   * structure.
   */
  case object Evolve

  /**
   * Response to Evolve, saying that it is complete.
   */
  case object Evolved

  /**
   * Fetch the owning User of this Space.
   */
  case object GetOwner

  case class SpaceOwner(ownerIdentity: OID)

  /**
   * Tells the Persister to clear out any cache it is maintaining. Usually comes before a fresh Load.
   */
  case object Clear

  /**
   * Request from the Space to the Persister, send when the Space is booted. Persister should
   * respond with Loaded.
   */
  case class Load(apps: Seq[SpaceState])

  /**
   * Response sent when the Persister successfully has loaded the Space.
   */
  case class Loaded(state: SpaceState)

  /**
   * Signifies that this Space doesn't exist in the old MySQL-based system.
   */
  case object NoOldSpace

  /**
   * Command to delete the Thing with the specified OID. Fire-and-forget, with no response.
   */
  case class Delete(thingId: OID)

  case class SpaceChange(
    newName: String,
    newDisplay: String
  )

  /**
   * Command to alter the specified Thing. spaceChange should be given iff the Thing is the Space
   * itself. Should response with Changed().
   */
  case class Change(
    state: SpaceState,
    thingId: OID,
    modelId: OID,
    modTime: DateTime,
    props: PropMap,
    spaceChange: Option[SpaceChange]
  )

  /**
   * Command to create a new Thing.
   */
  case class Create(
    state: SpaceState,
    modelId: OID,
    kind: Kind,
    props: PropMap,
    modTime: DateTime
  )

  /**
   * Response from a Change() or Create().
   */
  case class Changed(
    thingId: OID,
    timestamp: DateTime
  )

  // =====================================
  //
  // Space Manager Persistence
  //

  /**
   * Command to create a new Space. Note that ownerId should be an *Identity*.
   *
   * spaceId is the Space to be created. The caller should have already allocated this OID. (We do it
   * this way because sometimes we care about allocating it first.)
   *
   * userMaxSpaces is the maximum number of Spaces this owner is allowed to have; this will be
   * checked before creation.
   *
   * name is the canonical name of the Space; display is the display name.
   *
   * This will return either a Changed(), or ThingError.
   */
  case class CreateSpacePersist(
    ownerId: OID,
    spaceId: OID,
    userMaxSpaces: Int,
    name: String,
    display: String,
    initialStatus: SpaceStatusCode
  )

  /**
   * This is a variant of CreateSpacePersist, that checks whether the Space already exists, and creates it if not.
   * It is intended for creating standard "system" Spaces that are hardcoded in.
   *
   * Responds with Changed() if it created the Space, or NoChangeNeeded() if not.
   */
  case class CreateSpaceIfMissing(
    ownerId: OID,
    spaceId: OID,
    userMaxSpaces: Int,
    name: String,
    display: String,
    initialStatus: SpaceStatusCode
  )

  case class NoChangeNeeded(thingId: OID)

  /**
   * Command to fetch a Space by its path. OwnerId should be an Identity.
   *
   * This will return either a SpaceId(), or ThingError.
   */
  case class GetSpaceByName(
    ownerId: OID,
    name: String
  )

  /**
   * The general error response when things go wrong. This probably needs to become more
   * complex as we go along.
   */
  case object PersistError
}
