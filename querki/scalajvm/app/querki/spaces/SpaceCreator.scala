package querki.spaces

import akka.actor._

import org.querki.requester._

import querki.globals._
import querki.identity.User
import querki.spaces.messages.ThingError
import querki.util.PublicException

import PersistMessages._

trait SpaceCreator extends EcologyMember with RequesterImplicits {

  private lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]

  def persister: ActorRef
  def context: ActorContext

  lazy val maxSpaces = Config.getInt("querki.public.maxSpaces", 5)

  // Any checks we can make without needing to go to the DB should go here. Note that we
  // intentionally don't do any legality checking on the name yet -- since it is a display name,
  // we're pretty liberal about what's allowed.
  private def checkLegalSpaceCreation(
    owner: User,
    display: String
  ): Option[Exception] = {
    if (!owner.canOwnSpaces)
      Some(PublicException("Space.create.pendingUser"))
    else
      None
  }

  /**
   * Actually creates a new Space in the database, but does *not* boot it up -- that's for the calling
   * code to deal with.
   */
  def createSpace(
    user: User,
    spaceId: OID,
    name: String,
    display: String,
    initialStatus: SpaceStatusCode
  ): RequestM[OID] = {
    checkLegalSpaceCreation(user, display) match {
      case Some(ex) => RequestM.failed(ex)
      case None => {
        val userMaxSpaces = {
          if (user.isAdmin || user.level == querki.identity.UserLevel.PermanentUser)
            Int.MaxValue
          else
            maxSpaces
        }

        for {
          persistResponse <- persister.request(CreateSpacePersist(
            user.mainIdentity.id,
            spaceId,
            userMaxSpaces,
            name,
            display,
            initialStatus
          ))
        } yield persistResponse match {
          case Changed(_, _)     => spaceId
          case ThingError(ex, _) => throw ex
        }
      }
    }
  }
}
