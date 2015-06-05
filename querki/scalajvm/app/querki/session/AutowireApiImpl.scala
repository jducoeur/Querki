package querki.session

import scala.concurrent.{Future, Promise}
import akka.actor._

import rx._

import org.querki.requester._

import models.{Thing, ThingId}

import querki.globals._

import querki.data.TID
import querki.identity.User
import querki.spaces.messages.SessionRequest
import querki.values.{RequestContext, SpaceState}

import messages.SessionMessage

/**
 * Passthrough parameters. The subclass of AutowireApiImpl should accept these and pass them into
 * AutowireApiImpl, but the subclass should note directly use these. Instead, use the accessors
 * built into AutowireApiImpl itself.
 */
case class AutowireParams(
  /**
   * The User who is making this request.
   */
  user:User,
  
  /**
   * The current state of the Space, as seen by this User.
   */
  state:SpaceState,
  
  /**
   * The RequestContext for this request.
   */
  rc:RequestContext,
  
  /**
   * The router to *this* Space, which we use to send messages to other members of the Troupe.
   */
  spaceRouter:ActorRef,
  
  /**
   * The actor we should use to send messages.
   */
  actor:Actor with Stash with Requester
)

/**
 * Base class for implementations of Autowire classes that hook into the UserSpaceSession.
 * An instance of the relevant class will be created for *each* method invocation, so that
 * we can stick the contextual information into the params and have it stick around until
 * the call completes. (Which will often involve Futures.)
 * 
 * EXTREMELY IMPORTANT: the consequence of this is that you must *never* create any persistent
 * pointers to instances of this class!!! If you do, they will become enormous memory leaks!
 * Remember, functional programming is your friend...
 */
class AutowireApiImpl(info:AutowireParams, val ecology:Ecology) extends EcologyMember with RequesterImplicits {
  def user = info.user
  def state = info.state
  val curThingRx = Var[Option[Thing]](None)
  val rcRx = Rx { curThingRx().map(thing => info.rc + thing).getOrElse(info.rc) }
  def rc = rcRx()
  def self = info.actor.self
  val spaceRouter = info.spaceRouter
  val requester = info.actor
  
  def withThing[R](thingId:TID)(f:Thing => R):R = {
    val oid = ThingId(thingId.underlying)
    // Either show this actual Thing, or a synthetic TagThing if it's not found:
    val thing = state.anything(oid).getOrElse(interface[querki.tags.Tags].getTag(thingId.underlying, state))
    curThingRx() = Some(thing)
    f(thing)
  }
  
  /**
   * Constructs a request suitable for looping back to the UserSpaceSession.
   */
  def createSelfRequest(payload:SessionMessage):SessionRequest = {
    SessionRequest(user, state.id, payload)
  }
  
  implicit def thing2TID(t:Thing):TID = TID(t.id.toThingId)
  implicit def OID2TID(oid:OID):TID = TID(oid.toThingId)
  implicit class TIDExt(tid:TID) {
    def toThingId = ThingId(tid.underlying)
  }
}
