package querki.identity

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import querki.ecology._

import IdentityCache._

class IdentityEcot(e:Ecology) extends QuerkiEcot(e) with IdentityAccess {
  
  /**
   * The one true handle to the Identity Cache.
   */
  var _ref:Option[ActorRef] = None
  lazy val identityCache = _ref.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new IdentityCache(ecology)), "IdentityCache")
  }
  
  implicit val cacheTimeout = Timeout(5 seconds)

  def getIdentity(id:OID):Future[Option[PublicIdentity]] = {
    val fut = identityCache ? GetIdentityRequest(id)
    fut map {
      case IdentityFound(identity) => Some(identity)
      case IdentityNotFound => None
      case _ => None
    }
  }
  
  def getIdentities(ids:Seq[OID]):Future[Map[OID, PublicIdentity]] = {
    val fut = identityCache ? GetIdentities(ids)
    fut map {
      case IdentitiesFound(identities) => identities
      case _ => Map.empty
    }    
  }
}