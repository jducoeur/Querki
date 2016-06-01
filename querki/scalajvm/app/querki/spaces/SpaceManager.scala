package querki.spaces

import language.postfixOps
import scala.util._

import org.querki.requester._

import models.{AsName, AsOID, OID}
import models.{Kind, Thing}
import messages._
import SpaceError._

import querki.db.ShardKind
import ShardKind._

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import anorm.{Success=>AnormSuccess,_}
import play.api._
import play.api.db._
import play.api.libs.concurrent._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play.current
import play.Configuration

import querki.core.NameUtils
import querki.ecology._

import querki.identity.User

import querki.spaces._

import querki.util._
import querki.util.SqlHelpers._

import PersistMessages._

/**
 * TODO: all the features here probably can and should be moved out of this singleton, and be handled
 * from UserSession instead. Provided we can hook UserSession to the pool of Persisters, that ought to just
 * work.
 */
class SpaceManager(e:Ecology, val region:ActorRef) extends Actor with Requester with EcologyMember {
  
  implicit val ecology = e
  
  lazy val persistenceFactory = interface[SpacePersistenceFactory]
  
  /**
   * This Actor deals with all DB-style operations for the SpaceManager.
   */
  lazy val persister = persistenceFactory.getSpaceManagerPersister
  
  def receive = {
    // This is entirely a DB operation, so just have the Persister deal with it:
    case req @ ListMySpaces(owner) => persister.forward(req)
    
//    TODO: how do we implement this in Cluster Sharding? Current theory is that we'll use
//    Distributed PubSub, with each SpaceRouter subscribing and the Admin system sending.
//    case req @ GetSpacesStatus(requester) => {
//      if (requester.isAdmin) {
//        // Each Space responds for itself:
//        children.foreach(space => space.forward(req))
//      } else {
//        QLog.error("Illegal request for GetSpacesStatus, from user " + requester.id)
//      }
//    } 
    
    case req @ GetSpaceCount(requester) => {
      if (requester.isAdmin) {
        persister.forward(req)
      } else {
        QLog.error("Illegal request for GetSpaceCount, from user " + requester.id)        
      }
    }

    case req @ CreateSpace(requester, display) => {
      Tryer 
        { checkLegalSpaceCreation(requester,display) } 
        { unit =>
          val userMaxSpaces = {
            if (requester.isAdmin || requester.level == querki.identity.UserLevel.PermanentUser)
              Int.MaxValue
            else
              maxSpaces
          }
          
          val canon = NameUtils.canonicalize(display)
          persister.request(CreateSpacePersist(requester.mainIdentity.id, userMaxSpaces, canon, display)) foreach {
            case err:ThingError => sender ! err
            case Changed(spaceId, _) => sender ! SpaceInfo(spaceId, canon, display, requester.mainIdentity.handle)
          }
        }  
        { sender ! ThingError(_) }
    }
    
    case req:GetSpaceByName => persister.forward(req)
    case req:ArchiveSpace => persister.forward(req)
  }
  
  // Any checks we can make without needing to go to the DB should go here. Note that we
  // intentionally don't do any legality checking on the name yet -- since it is a display name,
  // we're pretty liberal about what's allowed.
  private def checkLegalSpaceCreation(owner:User, display:String):Unit = {
    if (!owner.canOwnSpaces)
      throw new PublicException("Space.create.pendingUser")
  }
  
  lazy val maxSpaces = Config.getInt("querki.public.maxSpaces", 5)
}