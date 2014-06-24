package querki.identity

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Await, Future}

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import play.api.mvc.{RequestHeader, Security}

import models.{SimplePTypeBuilder, ThingState, Wikitext}

import querki.ecology._
import querki.util.ActorHelpers._
import querki.values.{QLContext, SpaceState}
import querki.util.QLog

import IdentityCacheMessages._
import UserCacheMessages._

object IdentityMOIDs extends EcotIds(39) {
  val IdentityTypeOID = moid(1)  
  val DisplayIdentityOID = moid(2)
  val ResolveIdentityOID = moid(3)
}

class IdentityEcot(e:Ecology) extends QuerkiEcot(e) with IdentityAccess with querki.core.MethodDefs {
  
  import IdentityMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  
  /**
   * The one true handle to the Identity Cache.
   */
  var _ref:Option[ActorRef] = None
  lazy val identityCache = _ref.get
  
  var _userRef:Option[ActorRef] = None
  lazy val userCache = _userRef.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new IdentityCache(ecology)), "IdentityCache")
    _userRef = createActorCb(Props(new UserCache(ecology)), "UserCache")
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
  
  def invalidateCache(id:OID):Unit = {
    identityCache ! InvalidateCacheForIdentity(id)
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
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  /**
   * TODO: this page calls a couple of Highly Evil functions. It works, but blocks far too much.
   */
  lazy val DisplayIdentity = ThingState(DisplayIdentityOID, systemOID, querki.basic.MOIDs.SimpleThingOID,
    toProps(
      setName("_displayIdentity"),
      Basic.DisplayTextProp("""[[$identity -> _asType(Link Type) -> _resolveIdentity -> ""
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
    def doWikify(context:QLContext)(v:PublicIdentity, displayOpt:Option[Wikitext] = None) = {
      Wikitext(s"[${v.name}](_displayIdentity?identity=${v.id})")
    }
    
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
    override def qlApply(inv:Invocation):QValue = {
      for {
        link <- inv.contextAllAs(Core.LinkType)
        // EEEEVIL!
        identity <- inv.opt(Await.result(getIdentity(link), (5 seconds)))
      }
        yield ExactlyOne(IdentityType(identity))
    }    
  }
  
  override lazy val props = Seq(
    ResolveIdentity
  )
}