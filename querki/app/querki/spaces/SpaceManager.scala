package querki.spaces

import language.postfixOps
import scala.util._

import models.{AsName, AsOID, OID}
import models.{Kind, Thing}
import models.system.OIDs._
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
  import models.system.SystemSpace
  import SystemSpace._
  import Space._
  
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

object SpaceManager {
  // I don't love having to hold a static reference like this, but Play's statelessness
  // probably requires that. Should we instead be holding a Path, and looking it up
  // each time?
  var _ref:Option[ActorRef] = None
  def setSpaceManager(r:ActorRef) = { _ref = Some(r) }
  def ref = _ref.get
  
  // This is probably over-broad -- we're going to need the timeout to push through to
  // the ultimate callers.
  implicit val timeout = Timeout(5 seconds)
  
  // Send a message to the SpaceManager, expecting a return of type A to be
  // passed into the callback. This wraps up the messy logic to go from a
  // non-actor-based Play environment to the SpaceManager. We'll likely
  // generalize it further eventually.
  //
  // Type A is the response we expect to get back from the message, which will
  // be sent to the given callback.
  //
  // Type B is the type of the callback. I'm a little surprised that this isn't
  // inferred -- I suspect I'm doing something wrong syntactically.
  def ask[A,B](msg:SpaceMgrMsg)(cb: A => B)(implicit m:Manifest[A]):Future[B] = {
    // Why isn't this compiling properly? We should be getting an implicit import of ?
//    (ref ? msg).mapTo[A].map(cb)
    akka.pattern.ask(ref, msg).mapTo[A].map(cb)
  }
}
