package querki.identity

import akka.actor.{ActorRef, Props}
import akka.cluster.sharding._
import akka.pattern.ask
import akka.util.Timeout

import play.api.mvc.{RequestHeader, Security}

import models._

import querki.ecology._
import querki.email.EmailAddress
import querki.globals._
import querki.session.UserSessionMessages.UserSessionMsg
import querki.system.TOSModule.noTOSUserVersion
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
  
  import MOIDs._
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
  val identityExtractor:ShardRegion.ExtractEntityId = {
    case msg:IdentityRequest => (msg.id.shard, msg) 
  }
  val identityResolver:ShardRegion.ExtractShardId = msg => msg match {
    case msg:IdentityRequest => msg.id.shard
  }
  
  // These tell how to shard the UserCache. Similarly to identity, there is a single entity per
  // shard, but in this case it's based on the handle in the message.
  def userShard(msg:UserCacheRequest) = (msg.handle.hashCode() % 30).toString
  val userExtractor:ShardRegion.ExtractEntityId = {
    case msg:UserCacheRequest => (userShard(msg), msg) 
  }
  val userResolver:ShardRegion.ExtractShardId = msg => msg match {
    case msg:UserCacheRequest => userShard(msg)
  }  
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _ref = SystemManagement.createShardRegion("IdentityCache", IdentityCache.actorProps(ecology), 
        identityExtractor, identityResolver)
    _userRef = SystemManagement.createShardRegion("UserCache", UserCache.actorProps(ecology), 
        userExtractor, userResolver)
  }
  
  override def persistentMessages = persist(39,
    (classOf[IdentityPersistence.UserRef] -> 100)
  )

  // Internal System User. This should be used for making internal changes to Spaces that are *not* under the
  // aegis of the requesting user. 
  //
  // USE WITH EXTREME CAUTION! Don't mess with this if you don't understand it! The SystemUser has essentially
  // unlimited rights, so should only be invoked when we are intentionally doing something on the user's
  // behalf that they cannot do themselves. That automatically requires a security audit.
  case object SystemUser extends User {
    val id = SystemUserOID
    val name = "SystemUser"
    lazy val email = EmailAddress(Config.getString("querki.mail.systemFrom", "querki@querki.net"))
    // TODO: the presence of a reference to models.system here is suspicious. Does this indicate that SystemUser
    // doesn't belong in this file? Likely so.
    val identities = Seq(Identity(SystemIdentityOID, email, "", "systemUser", name, IdentityKind.QuerkiLogin))
    val level = UserLevel.SuperadminUser
    val tosVersion = noTOSUserVersion
  }
  
  implicit val cacheTimeout = defaultTimeout

  def getIdentity(id:OID):Future[Option[PublicIdentity]] = {
    getIdentityInternal(id)
  }
  
  def getIdentityInternal(id:OID):Future[Option[FullIdentity]] = {
    val fut = identityCache ? GetIdentityRequest(id)
    fut map {
      case IdentityFound(identity) => Some(identity)
      case IdentityNotFound(_) => None
      case _ => None
    }
  }
  
  def getIdentity(handle:String):Future[Option[PublicIdentity]] = {
    val fut = userCache askRetry GetUserByHandle(handle)
    fut map {
      case UserFound(user) => {
        user.identityByHandle(handle)
      }
      case UserNotFound => None
      case _ => None
    }
  }
  
  def getFullIdentity(id:OID):Future[Option[FullIdentity]] = {
    getIdentityInternal(id)
  }

  // TODO: this is too important a function to be this inefficient and unreliable. It works like this because the
  // IdentityCache is distributed around the cluster, but that's probably a bad design. Would it be possible
  // to redo the cache as a CRDT of some sort instead? Needs some research, and some thought about whether we
  // can cope with the cache sometimes containing stale data. My *guess* is that that's okay for any use case
  // that is getting many Identities. It may well be that we need two caches: a distributed high-reliability one
  // and a CRDT one that can be a bit stale.
  def getIdentitiesInternal[T >: FullIdentity](ids:Seq[OID], notFound:(Map[OID, T], OID) => Map[OID, T]):Future[Map[OID, T]] = {
    val requests:Set[Future[Any]] = ids.toSet.map { id:OID => identityCache ? GetIdentityRequest(id) }
    val resultSetFut = Future.sequence(requests)
    resultSetFut.map { resultSet =>
      (Map.empty[OID, T] /: resultSet) { (m, response) =>
        response match {
          case IdentityFound(identity) => m + (identity.id -> identity)
          case IdentityNotFound(identityId) => notFound(m, identityId)
        }
      }
    } 
  }
  
  /**
   * Note that, when fetching *Public* Identities, this will synthesize Guest IDs for any that aren't found,
   * since that is generally what we want. If the caller cares, they should screen for Trivial Identities.
   */
  def getIdentities(ids:Seq[OID]):Future[Map[OID, PublicIdentity]] = {
    getIdentitiesInternal[PublicIdentity](ids, (m, id) => m + (id -> makeTrivial(id).mainIdentity))
  }
  
  def getFullIdentities(ids:Seq[OID]):Future[Map[OID, FullIdentity]] = {
    getIdentitiesInternal[FullIdentity](ids, (m, _) => m)
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
        val fut = userCache askRetry GetUserByHandle(username)
        fut map {
          case UserFound(user) => Some(user)
          case _ => None
        }
      }
      case None => {
        // There isn't a User. Is there a Guest?
        fut(guestFromSession(request))
      }
    }
  }
  
  def guestFromSession(request:RequestHeader):Option[User] = {
    (request.session.get(User.guestIdSessionParam), request.session.get(User.guestEmailSessionParam)) match {
      case (Some(identityId), Some(emailAddr)) => {
        Some(makeGuest(identityId, emailAddr))
      }
      case (Some(identityId), None) => {
        Some(makeTrivial(OID(identityId)))
      }
      // Nope -- there's no Session here:
      case _ => None
    }
  }
  
  def makeGuest(identityIdStr:String, emailAddrStr:String):User = {
    GuestUser(Identity(OID(identityIdStr), EmailAddress(emailAddrStr), "", "", s"Guest $emailAddrStr", IdentityKind.SimpleEmail))
  }
  
  def makeTrivial(identityId:OID):User = {
    GuestUser(Identity(identityId, EmailAddress(""), "", "", s"Guest User", IdentityKind.Trivial))
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
  
  lazy val ResolveIdentity = new InternalMethod(ResolveIdentityOID,
    toProps(
      setName("_resolveIdentity"),
      Summary("Given a Link to an Identity, this fetches that Identity")))
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
