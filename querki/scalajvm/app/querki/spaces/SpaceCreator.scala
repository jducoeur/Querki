package querki.spaces

import akka.actor.ActorRef

import org.querki.requester._

import querki.globals._
import querki.identity.User
import querki.spaces.messages.ThingError
import querki.util.PublicException

import PersistMessages._

trait SpaceCreator extends EcologyMember with RequesterImplicits {
  
  lazy val maxSpaces = Config.getInt("querki.public.maxSpaces", 5)
  
  def persister:ActorRef
  
  // Any checks we can make without needing to go to the DB should go here. Note that we
  // intentionally don't do any legality checking on the name yet -- since it is a display name,
  // we're pretty liberal about what's allowed.
  private def checkLegalSpaceCreation(owner:User, display:String):Option[Exception] = {
    if (!owner.canOwnSpaces)
      Some(PublicException("Space.create.pendingUser"))
    else
      None
  }

  /**
   * Actually creates a new Space in the database, but does *not* boot it up -- that's for the calling
   * code to deal with.
   */
  def createSpace(requester:User, name:String, display:String, initialStatus:SpaceStatusCode):RequestM[OID] = {
    checkLegalSpaceCreation(requester,display) match {
      case Some(ex) => RequestM.failed(ex)
      case None => {
        val userMaxSpaces = {
          if (requester.isAdmin || requester.level == querki.identity.UserLevel.PermanentUser)
            Int.MaxValue
          else
            maxSpaces
        }
        
        persister.request(CreateSpacePersist(requester.mainIdentity.id, userMaxSpaces, name, display, initialStatus)).map {
            case ThingError(ex, _) => throw ex
            case Changed(spaceId, _) => spaceId
        }
      }
    }
  }
}
