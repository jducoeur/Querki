package querki.api

import scala.concurrent.{Promise}
import scala.util.{Failure, Success}
import akka.actor._
import upickle.default._
import autowire._

import rx._

import org.querki.requester._

import models.{Thing, ThingId}

import querki.data.TID
import querki.globals._
import querki.identity.User
import querki.session.messages.SessionMessage
import querki.spaces.messages.SpaceSubsystemRequest
import querki.streaming._
import querki.util.UnexpectedPublicException
import querki.values.{RequestContext, SpaceState}

/**
 * Passthrough parameters. The subclass of AutowireApiImpl should accept these and pass them into
 * AutowireApiImpl, but the subclass should note directly use these. Instead, use the accessors
 * built into AutowireApiImpl itself.
 * 
 * TODO: investigate how to make the payload more strongly-typed, with enforcement that the right
 * API gets the right payload type, with a minimum of boilerplate. May require playing with Shapeless
 * to get this right, I suspect.
 */
case class AutowireParams(
 /**
   * The User who is making this request.
   */
  user:User,
  
  /**
   * The payload -- information particular to the invoking context.
   */
  payload:Option[Any],
  
  /**
   * The RequestContext for this request.
   */
  rc:RequestContext,
  
  /**
   * The actor we should use to send messages.
   */
  actor:Actor with Requester,
  
  /**
   * The sender who invoked this request.
   */
  sender:ActorRef
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
abstract class AutowireApiImpl(info:AutowireParams, e:Ecology) extends EcologyMember with RequesterImplicits 
  with autowire.Server[String, Reader, Writer]
{
  def user = info.user
  def rc = info.rc
  def self = info.actor.self
  implicit def context = info.actor.context
  def sender = info.sender
  def requester = info.actor
  
  implicit lazy val ecology = e
  
  /***************************************************
   * Wrapping code
   */
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.default.write(r)
  def read[Result: Reader](p: String) = upickle.default.read[Result](p)
  
  def handleException(th:Throwable, req:Request) = {
    def apiName = req.path(2)
    th match {
      case aex:querki.api.ApiException => {
        sender ! ClientError(write(aex))
      }
      case pex:PublicException => {
        QLog.error(s"$apiName replied with PublicException $th instead of ApiException when invoking $req")
        sender ! ClientError(pex.display(Some(rc)))
      }
      case ex:Exception => {
        QLog.error(s"Got exception from $apiName when invoking $req", ex)
        sender ! ClientError(UnexpectedPublicException.display(Some(rc)))                
      }
      case _ => {
        QLog.error(s"Got throwable from $apiName when invoking $req", th)
        sender ! ClientError(UnexpectedPublicException.display(Some(rc)))
      }
    }              
  }

  /**
   * Concrete implementation classes must define this. It's a bit boilerplatey, but necessary to make
   * the Autowire macros work. It should usually say:
   * {{{
   * def doRoute(req:Request) = route[ThingyFunctions](this)(req)
   * }}}
   */
  def doRoute(req:Request):Future[String]

  // TODO: this is duplicated from StringStreamSender; we should probably refactor these together
  // somewhere:
  val chunkSize = Config.getInt("querki.stream.stringChunkSize", 10000)
  
  def handleRequest(req:Request, completeCb: Any => Unit) = {
    try {
      doRoute(req).onComplete { 
        case Success(result) => {
          if (result.length > chunkSize) {
            // The result string is over-sized. We're going to stream it, so we don't hit inter-node
            // serialization limits. Fire up a StringSender, and pass its address along:
            val stringSender = context.actorOf(StringStream.senderProps(result))
            sender ! OversizedResponse(stringSender)
            stringSender.request(StringStream.Start).map {
              case StringStream.SendComplete => {
                completeCb(result) 
              }
            }
          } else {
            sender ! ClientResponse(result)
            completeCb(result) 
          }
        }
        case Failure(ex) => { handleException(ex, req); completeCb(ex) }
      }
    } catch {
      case ex:Throwable => { handleException(ex, req); completeCb(ex) }
    }
  }
  
  implicit def thing2TID(t:Thing):TID = TID(t.id.toThingId)
  implicit def OID2TID(oid:OID):TID = TID(oid.toThingId)
  implicit class TIDExt(tid:TID) {
    def toThingId = ThingId(tid.underlying)
  }
}

case class SpacePayload(state:SpaceState, spaceRouter:ActorRef)

abstract class SpaceApiImpl(info:AutowireParams, e:Ecology) extends AutowireApiImpl(info, e) {
  // HACK: this is the motivation for building the payload type into AutowireParams:
  def payload = info.payload.get.asInstanceOf[SpacePayload]
  def state = payload.state
  def spaceRouter = payload.spaceRouter
  // TBD: we have both this and rc.isOwner. I like this definition more, but it's not always available. 
  // Hmm...
  def isOwner = user.hasIdentity(state.owner)
  
  def withThing[R](thingId:TID)(f:Thing => R):R = {
    val oid = ThingId(thingId.underlying)
    // Either show this actual Thing, or a synthetic TagThing if it's not found:
    val thing = state.anything(oid).getOrElse(interface[querki.tags.Tags].getTag(thingId.underlying, state))
    f(thing)
  }
  
  def withProp[R](propId:TID)(f:AnyProp => R):R = {
    val oid = ThingId(propId.underlying)
    val prop = state.prop(oid).get
    f(prop)
  }
  
  /**
   * Constructs a request suitable for looping back to the UserSpaceSession.
   * 
   * TODO: can we refactor this out of the general AutowireApiImpl?
   */
  def createSelfRequest(msg:SessionMessage):SpaceSubsystemRequest = {
    SpaceSubsystemRequest(rc, state.id, msg)
  }

}
