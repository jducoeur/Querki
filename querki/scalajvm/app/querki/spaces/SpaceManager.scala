package querki.spaces

import language.postfixOps
import scala.util._

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

class SpaceManager(val ecology:Ecology) extends Actor with Requester with EcologyMember with RoutingParent[OID] {
  
  lazy val persistenceFactory = interface[SpacePersistenceFactory]
  
  /**
   * This Actor deals with all DB-style operations for the SpaceManager.
   */
  lazy val persister = persistenceFactory.getSpaceManagerPersister
  
  // Cache of the mapping from Space Name to OID.
  // TODO: forcibly expire this cache on Space Name change.
  private case class SpaceInfo(id:OID, name:String)
  private var spaceNameCache:Map[String,SpaceInfo] = Map.empty
  
  def createChild(key:OID):ActorRef = context.actorOf(SpaceRouter.actorProps(ecology, persistenceFactory, key), sid(key))
  
  def receive = {
    // This is entirely a DB operation, so just have the Persister deal with it:
    case req @ ListMySpaces(owner) => persister.forward(req)
    
    case req @ GetSpacesStatus(requester) => {
      if (requester.isAdmin) {
        // Each Space responds for itself:
        children.foreach(space => space.forward(req))
      } else {
        QLog.error("Illegal request for GetSpacesStatus, from user " + requester.id)
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
          
          persister.request(CreateSpacePersist(requester.mainIdentity.id, userMaxSpaces, NameUtils.canonicalize(display), display)) {
            case err:ThingError => sender ! err
            // Now, let the Space Actor finish the process once it is ready:
            case Changed(spaceId, _) => routeToChild(spaceId, req)
          }
        }  
        { sender ! ThingError(_) }
    }

    case req:SpaceMessage => {
      req match {
        case SpaceMessage(_, _, AsOID(spaceId)) => routeToChild(spaceId, req)
        case SpaceMessage(_, ownerId, AsName(spaceName)) => {
          val canonName = NameUtils.canonicalize(spaceName)
          spaceNameCache.get(canonName) match {
            case Some(SpaceInfo(spaceId, name)) => routeToChild(spaceId, req)
            case None => {
	          persister.request(GetSpaceByName(ownerId, canonName)) {
	            case SpaceId(spaceId) => {
	              routeToChild(spaceId, req)
	              spaceNameCache = spaceNameCache + (canonName -> SpaceInfo(spaceId, canonName))
	            }
	            case err:ThingError => sender ! err
	          }
            }
          }
        }
        // I sincerely don't believe this case is possible, but Scala is now insisting that the match is
        // not exhaustive without it:
        case _ => QLog.error("Got a weirdly formed SpaceMessage! This should be impossible, shouldn't it?")
      }
    }
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