package querki.identity

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.{ClusterSharding, ShardRegion}
import akka.pattern.ask
import akka.util.Timeout

import play.api.mvc.{RequestHeader, Security}

import models.{PropertyBundle, SimplePTypeBuilder, ThingState, Wikitext}

import querki.ecology._
import querki.globals._
import querki.session.UserSessionMessages.UserSessionMsg
import querki.util.ActorHelpers._
import querki.values.{EmptyValue, QLContext, SpaceState}
import querki.util.QLog

import IdentityCacheMessages._
import UserCacheMessages._

object IdentityMOIDs extends EcotIds(39) {
  val IdentityTypeOID = moid(1)  
  val DisplayIdentityOID = moid(2)
  val ResolveIdentityOID = moid(3)
}

private [identity] trait UserCacheAccess extends EcologyInterface {
  /**
   * This is used by UserPersistence to kick the UserCache. Note that this returns a Future, which will resolve after
   * the Cache has acknowledged the update.
   */
  def updateCacheAndThen(user:User):Future[Any]
}

class IdentityEcot(e:Ecology) extends QuerkiEcot(e) with IdentityAccess with querki.core.MethodDefs with UserCacheAccess {
  
  import IdentityMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]  
  val SystemManagement = initRequires[querki.system.SystemManagement]
  
  /**
   * The one true handle to the Identity Cache.
   */
  var _ref:Option[ActorRef] = None
  lazy val identityCache = _ref.get
  
  var _userRef:Option[ActorRef] = None
  lazy val userCache = _userRef.get
  
  // These two functions tell ClusterSharding the ID and shard for a given IdentityRequest.
  // Note that the name of the shard and the name of the entity are the same. That's intentional:
  // we want one entity -- one IdentityCache -- per shard.
  val identityExtractor:ShardRegion.IdExtractor = {
    case msg:IdentityRequest => (msg.id.shard, msg) 
  }
  val identityResolver:ShardRegion.ShardResolver = msg => msg match {
    case msg:IdentityRequest => msg.id.shard
  }
  
  // These tell how to shard the UserCache. Similarly to identity, there is a single entity per
  // shard, but in this case it's based on the handle in the message.
  def userShard(msg:UserCacheRequest) = (msg.handle.hashCode() % 30).toString
  val userExtractor:ShardRegion.IdExtractor = {
    case msg:UserCacheRequest => (userShard(msg), msg) 
  }
  val userResolver:ShardRegion.ShardResolver = msg => msg match {
    case msg:UserCacheRequest => userShard(msg)
  }  
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _ref = SystemManagement.createShardRegion("IdentityCache", IdentityCache.actorProps(ecology), 
        identityExtractor, identityResolver)
    _userRef = SystemManagement.createShardRegion("UserCache", UserCache.actorProps(ecology), 
        userExtractor, userResolver)
  }
  
  implicit val cacheTimeout = defaultTimeout

  def getIdentity(id:OID):Future[Option[PublicIdentity]] = {
    val fut = identityCache ? GetIdentityRequest(id)
    fut map {
      case IdentityFound(identity) => Some(identity)
      case IdentityNotFound => None
      case _ => None
    }
  }
  
  def getIdentity(handle:String):Future[Option[PublicIdentity]] = {
    val fut = userCache ? GetUserByHandle(handle)
    fut map {
      case UserFound(user) => {
        user.identityByHandle(handle)
      }
      case UserNotFound => None
      case _ => None
    }
  }
  
  def getIdentitiesInternal[T >: FullIdentity](ids:Seq[OID]):Future[Map[OID, T]] = {
    val requests:Set[Future[Any]] = ids.toSet.map { id:OID => identityCache ? GetIdentityRequest(id) }
    val resultSetFut = Future.sequence(requests)
    resultSetFut.map { resultSet =>
      (Map.empty[OID, T] /: resultSet) { (m, response) =>
        response match {
          case IdentityFound(identity) => m + (identity.id -> identity)
          case IdentityNotFound => m
        }
      }
    } 
  }
  
  def getIdentities(ids:Seq[OID]):Future[Map[OID, PublicIdentity]] = {
    getIdentitiesInternal[PublicIdentity](ids)
  }
  
  def getFullIdentities(ids:Seq[OID]):Future[Map[OID, FullIdentity]] = {
    getIdentitiesInternal[FullIdentity](ids)
  }  
  
  def invalidateCache(id:OID):Unit = {
    identityCache ! InvalidateCacheForIdentity(id)
  }
  
  def routeToUsers(identityIds:Seq[OID], router:ActorRef, msg:UserRouteableMessage):Unit = {
    identityIds.foreach(id => identityCache ! RouteToUser(id, router, msg))
  }
  
  /**
   * If we find the username in the current session, return a populated Some(User); otherwise, None.
   * 
   * TODO: cache the full record in the cookie! Note that this is closely related to User.toSession().
   */
  def userFromSession(request:RequestHeader):Future[Option[User]] = {
    request.session.get(Security.username) match {
      case Some(username) => {
        val fut = userCache ? GetUserByHandle(username)
        fut map {
          case UserFound(user) => Some(user)
          case _ => None
        }
      }
      case None => Future.successful(None)
    }
  }
  
  def updateCacheAndThen(user:User):Future[Any] = {
    val futs = user.identities.map { identity => userCache ? UpdateUser(identity.handle, user) }
    Future.sequence(futs)
  }
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val DisplayIdentity = ThingState(DisplayIdentityOID, systemOID, RootOID,
    toProps(
      setName("_displayIdentity"),
      Basic.DisplayTextProp("""[[$identity -> _asType(Thing Type) -> _resolveIdentity -> ""
          |### ____
          |
          |#### User Values
          |
          |[[_thingValues]]""]]""".stripMargin)))
          
  override lazy val things = Seq(
    DisplayIdentity
  )
  
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
    def doWikify(context:QLContext)(v:PublicIdentity, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      Future.successful(Wikitext(s"[${v.name}](_displayIdentity?identity=${v.id})"))
    }
    
    override def doComp(context:QLContext)(left:PublicIdentity, right:PublicIdentity):Boolean = {
      math.Ordering.String.lt(left.name, right.name) 
    }
    
    override def doMatches(left:PublicIdentity, right:PublicIdentity):Boolean = left.id == right.id 
    
    // None of these should be possible, so for now I'm not going to worry about them:
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:PublicIdentity)(implicit state:SpaceState) = ???
    def doDefault(implicit state:SpaceState) = ???
    
    def doComputeMemSize(v:PublicIdentity):Int = 0
  }
  
  override lazy val types = Seq(
    IdentityType
  )
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  /**
   * TODO: EEEEVIL! Contains a blocking call!
   */
  lazy val ResolveIdentity = new InternalMethod(ResolveIdentityOID,
    toProps(
      setName("_resolveIdentity"),
      Summary("Given a Link to an Identity, this fetches that Identity"),
      Details("This function is currently a bit problematic. Please don't over-use it. Eventually it will be less dangerous.")))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        link <- inv.contextAllAs(Core.LinkType)
        identityOpt <- inv.fut(getIdentity(link))
      }
        yield identityOpt match {
          case Some(identity) => ExactlyOne(IdentityType(identity))
          case None => EmptyValue(IdentityType)
        }
    }    
  }
  
  override lazy val props = Seq(
    ResolveIdentity
  )
}