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

class SpaceManager(val ecology:Ecology, persistenceFactory:SpacePersistenceFactory) extends Actor with Requester with EcologyMember {
  /**
   * This Actor deals with all DB-style operations for the SpaceManager.
   */
  lazy val persister = persistenceFactory.getSpaceManagerPersister
  
  // Cache of the mapping from Space Name to OID.
  // TODO: in the long-term architecture, we should *always* be putting stuff into this cache,
  // even when the original lookup wasn't by name. This cache will also be fully distributed.
  // TODO: expire entries from this cache, at least on inactivity.
  // TODO: forcibly expire this cache on Space Name change.
  private case class SpaceInfo(id:OID, name:String)
  private var spaceNameCache:Map[String,SpaceInfo] = Map.empty
  
  def getSpace(spaceId:OID):ActorRef = {
    val mySid = sid(spaceId)
    // Fetch the existing Space Actor, or fire it up:
    context.child(mySid).getOrElse(context.actorOf(Space.actorProps(ecology, persistenceFactory), mySid))
  }
  
  def receive = {
    // This is entirely a DB operation, so just have the Persister deal with it:
    case req @ ListMySpaces(owner) => persister.forward(req)

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
            case Changed(spaceId, _) => getSpace(spaceId).forward(req)
          }
        }  
        { sender ! ThingError(_) }
    }

    // TODO: we need a pseudo-Space for System!
    case req:SpaceMessage => {
      req match {
        case SpaceMessage(_, _, AsOID(spaceId)) => getSpace(spaceId).forward(req)
        case SpaceMessage(_, ownerId, AsName(spaceName)) => {
          val canonName = NameUtils.canonicalize(spaceName)
          spaceNameCache.get(canonName) match {
            case Some(SpaceInfo(spaceId, name)) => getSpace(spaceId).forward(req)
            case None => {
	          persister.request(GetSpaceByName(ownerId, canonName)) {
	            case SpaceId(spaceId) => {
	              getSpace(spaceId).forward(req)
	              spaceNameCache = spaceNameCache + (canonName -> SpaceInfo(spaceId, canonName))
	            }
	            case err:ThingError => sender ! err
	          }
            }
          }
        }
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