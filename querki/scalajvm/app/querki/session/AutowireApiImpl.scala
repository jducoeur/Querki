package querki.session

import akka.actor._

import rx._

import models.{Thing, ThingId}

import querki.globals._

import querki.identity.User
import querki.util.Requester
import querki.values.{RequestContext, SpaceState}

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

class AutowireApiImpl(info:AutowireParams, val ecology:Ecology) extends EcologyMember {
  def user = info.user
  def state = info.state
  val curThingRx = Var[Option[Thing]](None)
  val rcRx = Rx { curThingRx().map(thing => info.rc + thing).getOrElse(info.rc) }
  def rc = rcRx()
  val spaceRouter = info.spaceRouter
  
  def withThing[R](thingId:String)(f:Thing => R):R = {
    val oid = ThingId(thingId)
    // Either show this actual Thing, or a synthetic TagThing if it's not found:
    val thing = state.anything(oid).getOrElse(interface[querki.tags.Tags].getTag(thingId, state))
    curThingRx() = Some(thing)
    f(thing)
  }
  
  // TBD: this is basically copied out of Requester. It's bad coupling, but I'm not sure it's
  // worth polluting Requester with the notion of making this all delegatable.
  implicit class RequestableActorRef(a:ActorRef) {
    def request(msg:Any)(handler:Actor.Receive) = info.actor.doRequest(a, msg)(handler)
  }
}
