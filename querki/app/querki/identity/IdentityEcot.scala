package querki.identity

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import models.{SimplePTypeBuilder, Wikitext}

import querki.ecology._
import querki.values.{QLContext, SpaceState}

import IdentityCacheMessages._

object IdentityMOIDs extends EcotIds(39) {
  val IdentityTypeOID = moid(1)  
}

class IdentityEcot(e:Ecology) extends QuerkiEcot(e) with IdentityAccess {
  
  import IdentityMOIDs._
  
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
  
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val IdentityType = new SystemType[PublicIdentity](IdentityTypeOID,
    toProps(
      setName("_identityType"),
      setInternal,
      Summary("The Type of a Querki member, not necessarily a member of any particular Space.")))
    with SimplePTypeBuilder[PublicIdentity]
  {
    def doWikify(context:QLContext)(v:PublicIdentity, displayOpt:Option[Wikitext] = None) = Wikitext(v.name)
    
    override def doComp(context:QLContext)(left:PublicIdentity, right:PublicIdentity):Boolean = {
      math.Ordering.String.lt(left.name, right.name) 
    }
    
    override def doMatches(left:PublicIdentity, right:PublicIdentity):Boolean = left.id == right.id 
    
    // None of these should be possible, so for now I'm not going to worry about them:
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:PublicIdentity)(implicit state:SpaceState) = ???
    def doDefault(implicit state:SpaceState) = ???
  }
  
  override lazy val types = Seq(
    IdentityType
  )
}