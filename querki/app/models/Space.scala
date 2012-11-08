package models

import akka.actor._
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout
import play.api.Configuration
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent._
import play.Configuration

/**
 * A Space is the Querki equivalent of a database -- a collection of related Things,
 * Properties and Types.
 * 
 * Note that, just like everything else, a Space is a special sort of Thing. It can
 * have Properties (including user-defined ones), and can potentially inherit from a
 * Model.
 * 
 * A SpaceState is a Space at a specific point in time. Operations are usually performed
 * on a SpaceState, to keep them consistent. Changes are sent to the Space, which generates
 * a new SpaceState from them.
 * 
 * TODO: implement Space inheritance -- that is, Apps.
 */
case class SpaceState(
    s:OID, 
    m:ThingPtr,
    owner:OID,
    name:String,
    types:Map[OID, PType],
    spaceProps:Map[OID, Property],
    things:Map[OID, ThingState]) 
  extends Thing(s, s, m, Kind.Space) 
{
  def resolve[T <: ThingPtr](ptr:ThingPtr)(lookup: OID => T):T = {
    ptr match {
      case id:OID => lookup(id)
      // TODO: Ick -- this is evil, and suggests that the ThingPtr model is fundamentally
      // flawed still. The problem is, any given ThingPtr potentially covers a host of
      // subtypes; we only know from context that we're expecting a specific one.
      case _ => ptr.asInstanceOf[T]
    }
  }
  def typ(ptr:ThingPtr) = resolve(ptr) (types(_))
  def prop(ptr:ThingPtr) = resolve(ptr) (spaceProps(_))
  def thing(ptr:ThingPtr) = resolve(ptr) (things(_))
}

sealed trait SpaceMessage
case class Load() extends SpaceMessage

class Space extends Actor {
  def receive = {
    case Load => {}
  }
}

sealed trait SpaceMgrMsg
case class ListMySpaces(owner:OID) extends SpaceMgrMsg
case class GetSpace(id:OID) extends SpaceMgrMsg
// TEMP:
case class SaySomething(something:String) extends SpaceMgrMsg

sealed trait GetSpaceResponse
case class RequestedSpace(state:SpaceState) extends GetSpaceResponse
case class GetSpaceFailed(id:OID) extends GetSpaceResponse

sealed trait ListMySpacesResponse
case class MySpaces(spaces:Seq[(OID,String)]) extends ListMySpacesResponse

class SpaceManager extends Actor {
  
  // The local cache of Space States.
  // TODO: this needs to age properly.
  // TODO: this needs a cap of how many states we will try to cache.
  var spaceCache:Map[OID,SpaceState] = Map.empty
  
  def addState(state:SpaceState) = spaceCache += (state.id -> state)
  
  // The System Space is hardcoded, and we create it at the beginning of time:
  addState(system.SystemSpace.State)
  
  var counter = 0
  
  // TEMP:
  val replyMsg = Play.configuration.getString("querki.test.replyMsg").getOrElse("MISSING REPLY MSG!")
  
  def receive = {
    // TEMP:
    case msg:SaySomething => {
      counter += 1
      sender ! msg.something + replyMsg + counter
    }
    case req:ListMySpaces => {
      val results = spaceCache.values.filter(_.owner == req.owner).map(space => (space.id, space.name)).toSeq
      sender ! MySpaces(results)
    }
    case req:GetSpace => {
      val cached = spaceCache.get(req.id)
      if (cached.nonEmpty)
        sender ! RequestedSpace(cached.get)
      else
        sender ! GetSpaceFailed(req.id)
     }
  }
  
}

object SpaceManager {
  // I don't love having to hold a static reference like this, but Play's statelessness
  // probably requires that. Should we instead be holding a Path, and looking it up
  // each time?
  lazy val ref = Akka.system.actorOf(Props[SpaceManager], name="SpaceManager")
  
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
  def ask[A,B](msg:SpaceMgrMsg)(cb: A => B)(implicit m:Manifest[A]):Promise[B] = {
    (ref ? msg).mapTo[A].map(cb).asPromise
  }
}