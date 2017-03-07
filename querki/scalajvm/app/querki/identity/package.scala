package querki

import scala.concurrent.Future
import scala.util.Try

import akka.actor.ActorRef

// TODO: this is an unfortunate abstraction break:
import play.api.mvc.RequestHeader

import org.querki.requester._

import models.{OID, Property, PType, SimplePTypeBuilder, Thing, ThingId}

import querki.ecology._

import querki.core.QLText
import querki.email.EmailAddress
import querki.session.UserSessionMessages.UserSessionMsg
import querki.util.PublicException
import querki.values.{RequestContext, SpaceState}

package object identity {

  import IdentityPersistence._

  object MOIDs extends EcotIds(3)  {
    // The central hard-coded User and Identity IDs, which are used for system and testing purposes:
    val SystemUserOID = sysId(9)
    val TestUserOID = sysId(11)
    val PrototypeUserOID = sysId(31)
    val SystemIdentityOID = sysId(97)
    val TestIdentityOID = sysId(98)
    val PrototypeIdentityOID = sysId(99)
    
    val PersonOID = oldMoid(1)
    val InviteLinkCmdOID = oldMoid(2)
    val IdentityLinkOID = oldMoid(3)
    // DEAD:
    val ChromelessInviteLinkOID = oldMoid(4)
    val MeMethodOID = oldMoid(5)
    val SecurityPrincipalOID = oldMoid(6)
    // DEAD:
    val ChromelessInvitesOID = moid(7)
    val InviteTextOID = moid(8)
    val SpaceInviteOID = moid(9)
    val PersonIdentityFunctionOID = moid(10)
    
    val InvitationStatusOID = moid(11)
    val StatusInvitedOID = moid(12)
    val StatusRequestedOID = moid(13)
    val StatusMemberOID = moid(14)
    val StatusRejectedOID = moid(15)
    val InvitationStatusPropOID = moid(16)
    val GuestUserOID = moid(17)
    val IsSimpleGuestOID = moid(18)
  }
  
  val IdentityTag = "Users, Identities and Invitations"
  
  /**
   * The values that can be put into the SpaceMembership table.
   * 
   * Note that this is actually TINYINT, and must not be > 127!
   */
  type MembershipState = Int
  object MembershipState {
    val member:MembershipState = 0
    val owner:MembershipState = 1
  }
  
  /**
   * Type alias to clarify when we are working specifically with Identities.
   * 
   * TODO: learn more about scalaz/shapeless Type Tags, and see if we can use them to make this safer.
   */
  type IdentityId = OID
  
  /**
   * Type alias to clarify when we are working with Users.
   */
  type UserId = OID

  case class InvitationResult(invited:Seq[String], alreadyInvited:Seq[String])
  
  sealed trait ParsedInvitation
  case class SpecificInvitation(personId:OID, user:User) extends ParsedInvitation
  case class OpenInvitation(roleId:OID) extends ParsedInvitation
  
  // The cookie parameter that indicates the email address of the target identity. The
  // fact that we have to expose this here suggests we have an abstraction break to fix...
  val identityEmail = "identityEmail"
  val personParam = "person"
  val identityParam = "identity"
  
  trait Person extends EcologyInterface {
    def IdentityLink:Property[OID,OID]
    def InviteText:Property[QLText, String]    
    
    def sendValidationEmail(rc:RequestContext, email:EmailAddress, user:User):Future[Unit]
    def validateEmail(user:User, validationStr:String):Future[Boolean]
   
    def inviteMembers(rc:RequestContext, invitees:Seq[EmailAddress], collaboratorIds:Seq[OID], originalState:SpaceState):Future[InvitationResult]
    
    def acceptInvitation[B](rc:RequestContext, personId:OID)(cb:querki.spaces.messages.ThingResponse => Future[B])(implicit state:SpaceState):Option[scala.concurrent.Future[B]]
    def acceptOpenInvitation(rc:RequestContext, roleId:OID)(implicit state:SpaceState):Future[Option[PublicException]]
    
    def getPersonIdentity(person:Thing)(implicit state:SpaceState):Option[OID]
    def hasPerson(user:User, personId:OID)(implicit state:SpaceState):Boolean
    // Simply asks whether this is a Member of this Space
    def hasMember(identity:IdentityId)(implicit state:SpaceState):Boolean
    def isPerson(identityId:OID, personId:OID)(implicit state:SpaceState):Boolean
    def isPerson(identity:Identity, person:Thing)(implicit state:SpaceState):Boolean
    /**
     * Returns the Identities (if any) that are Members of this Space.
     */
    def localIdentities(user:User)(implicit state:SpaceState):Iterable[Identity]
    def localPerson(identity:Identity)(implicit state:SpaceState):Option[Thing]
    def localPerson(identity:IdentityId)(implicit state:SpaceState):Option[Thing]
      
    /**
     * All the people who have been invited into this Space.
     */
    def people(implicit state:SpaceState):Iterable[Thing]
    /**
     * All the people who have been invited into this Space who have not yet accepted.
     */
    def invitees(implicit state:SpaceState):Iterable[Thing]
    /**
     * All the people who have joined this Space.
     */
    def members(implicit state:SpaceState):Iterable[Thing]
    
    def user2Ref(user:User)(implicit state:SpaceState):UserRef
    
    /**
     * True iff this Person record has been offered and accepted membership in this Space.
     * (Or requested and been accepted -- same difference.)
     */
    def isAcceptedMember(person:Thing)(implicit state:SpaceState):Boolean
    
    def replacePerson(guestId:OID, actualId:PublicIdentity)(implicit state:SpaceState, requester:Requester):RequestM[Any]
    
    private [identity] def withCache[T](f:CachedPeople => T)(implicit state:SpaceState):T
  }
  
  /**
   * A UserRouteableMessage can be passed into IdentityAccess.routeToUsers, and will be sent to all of
   * the specified Identities, by User. The key notion here is that it must provide a method that copies
   * this message with the given UserId.
   */
  trait UserRouteableMessage {
    def userId:UserId
    def toUser(userId:UserId):UserRouteableMessage
  }
  
  /**
   * Provides a cached front end to working with Identities. Note that this sits in front of an Actor, and
   * all calls are asynchronous!
   */
  trait IdentityAccess extends EcologyInterface {
    /**
     * The Identity Cache Actor. Other Actors are allowed to make requests directly to that instead
     * of going through the Future-based wrappers below. Use the messages from the IdentityCache object.
     */
    def identityCache:ActorRef
    
    /**
     * The User Cache Actor. Other Actors are allowed to make requested directly to that instead
     * of going through the Future-based wrappers below. Use the messages from the UserCache object.
     */
    def userCache:ActorRef
    
    /**
     * Wraps the notion of Identity in a QL-compatible Type.
     */
    def IdentityType:PType[PublicIdentity] with SimplePTypeBuilder[PublicIdentity]
    
    /**
     * The recommended way to fetch a single Identity.
     */
    def getIdentity(id:OID):Future[Option[PublicIdentity]]

    /**
     * Fetch an Identity from its Handle.
     */
    def getIdentity(handle:String):Future[Option[PublicIdentity]]
    
    /**
     * Fetch the email address for a single Identity.
     * 
     * Use with care! This is *not* public information!
     */
    def getFullIdentity(id:OID):Future[Option[FullIdentity]]
    
    /**
     * Fetch a number of identities at once.
     * 
     * IMPORTANT: it is *absolutely forbidden* to cast this PublicIdentity to FullIdentity. Yes, it's true
     * now, but that may not continue in the future; we may well divide them for security.
     */
    def getIdentities(ids:Seq[OID]):Future[Map[OID, PublicIdentity]]
    
    /**
     * Obtain the FullIdentity views of these identities. Only allowed within the Identity system itself,
     * to prevent leakage.
     */
    private [identity] def getFullIdentities(ids:Seq[OID]):Future[Map[OID, FullIdentity]]
    
    /**
     * Creates a GuestUser.
     */
    private [identity] def makeGuest(identityIdStr:String, emailAddrStr:String):User
    
    def makeTrivial(identityId:OID):User
    
    /**
     * Send the given message to the given router for all the provided IDs.
     */
    def routeToUsers(identityIds:Seq[OID], router:ActorRef, msg:UserRouteableMessage):Unit
    
    /**
     * Tells the system that, if this Identity is currently cached, we should clear that cache and reload it.
     * 
     * IMPORTANT: this is a fire-and-forget asynchronous call! Do not count upon it instantly taking effect!
     * It will take effect the next time someone tries to fetch this identity, but races can and will occur!
     */
    def invalidateCache(id:OID):Unit
    
    /**
     * Given the specified header from Play, fetch the User itself.
     */
    def userFromSession(req:RequestHeader):Future[Option[User]]
    
    /**
     * Iff there is a GuestUser in the current Session, return that Guest.
     */
    def guestFromSession(request:RequestHeader):Option[User]
    
    /**
     * Internal System User. This should be used for making internal changes to Spaces that are *not* under the
     * aegis of the requesting user. 
     * 
     * USE WITH EXTREME CAUTION! Don't mess with this if you don't understand it! The SystemUser has essentially
     * unlimited rights, so should only be invoked when we are intentionally doing something on the user's
     * behalf that they cannot do themselves. That automatically requires a security audit.
     */
    def SystemUser:User
  }
  
  trait NotifyInvitations extends EcologyInterface {
    private [identity] def notifyInvitation(req:User, textOpt:Option[QLText], invitees:Seq[FullIdentity])(implicit state:SpaceState):Unit
    
    def generateShareableLink(roleId:OID, state:SpaceState):String
    
    def parseInvite(encodedInvite:String):Option[ParsedInvitation]
  }
  
  /**
   * LOW-LEVEL INTERFACE.
   * 
   * This is full of blocking calls that go to the database. It should be considered deprecated for most
   * code. Use IdentityAccess instead where possible.
   * 
   * TODO: use of this trait outside of identity should now be considered a bug. Wrap all accesses behind
   * IdentityCache and UserCache instead!
   */
  trait UserAccess extends EcologyInterface {
    def addSpaceMembership(identityId:OID, spaceId:OID, membershipState:MembershipState = MembershipState.member):Boolean
    def changePassword(requester:User, identity:Identity, newPassword:String):Try[User]
    def changeDisplayName(requester:User, identity:Identity, newDisplay:String):Future[User]
    def changeUserLevel(userId:OID, requester:User, level:UserLevel.UserLevel):Future[Option[User]]
    def checkQuerkiLogin(login:String, passwordEntered:String):Option[User]
    def createUser(info:SignupInfo, confirmedEmail:Boolean, identityIdOpt:Option[OID] = None, identityExists:Boolean = true):Try[User]
    def get(request:RequestHeader):Option[User]
    
    // TODO: neither of these calls are scalable! We need to come up with better ways to implement both of
    // them, which will probably involve changing all calls to them!
    def getAllForAdmin(requester:User):Seq[User]
    def getAllIdsForAdmin(requester:User):Seq[UserId]
    def getPendingForAdmin(requester:User):Seq[User]
    
    def getByUserId(requester:User, userId:OID):Option[User]
    def getIdentity(rawHandle:String):Option[OID]
    def getIdentity(id:OID):Option[Identity]
    def getFullIdentity(id:IdentityId):Option[FullIdentity]
    def getIdentity(thingId:ThingId):Option[(Identity, UserLevel.UserLevel)]
    // WARNING: this should *not* often be used! It is dangerous from an Identity-security POV!
    def getUserByHandleOrEmail(raw:String):Option[User]
    def getUserByHandle(handle:String):Option[User]
    // The Future will resolve once the UserCache has had time to properly update.
    // TODO: any other entry points that change User state should be doing the same thing!
    def setTOSVersion(userId:OID, version:Int):Future[Option[User]]
    def getAcquaintanceIds(identityId:IdentityId):Seq[IdentityId]
    def getUserVersion(userId:UserId):Option[Int]
    // Intended for use when inviting somebody by email address. If this email address is already known,
    // that Identity is returned; otherwise, it creates a new, empty SimpleEmail Identity.
    def findOrCreateIdentityByEmail(email:String):FullIdentity
  }
}
