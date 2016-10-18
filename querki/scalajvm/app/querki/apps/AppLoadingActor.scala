package querki.apps

import scala.concurrent.duration._

import akka.actor._
import akka.util.Timeout

import org.querki.requester._

import models.{Property, ThingState}

import querki.globals._
import querki.identity.User
import querki.spaces.CacheUpdate
import querki.spaces.messages._
import querki.util.QuerkiActor

/**
 * The manager for loading a Space's Apps. For the time being, this is a (somewhat complex) one-shot worker.
 * 
 * This will probably get largely obviated by Akka Streams, but if not, think about switching it to Typed Akka
 * down the road.
 * 
 * TODO: I believe this code is now entirely dead.
 * 
 * @author jducoeur
 */
private [apps] class AppLoadingActor(ecology:Ecology, spaceId:OID, ownerIdentity:OID) extends QuerkiActor(ecology) {
  import AppLoadingActor._
  
  lazy val AppsPersistence = interface[AppsPersistence]
  
  // TODO: move this to Requester:
  // Adapted from the similar function in Future:
  def sequenceReqs[A, M[X] <: TraversableOnce[X]](seq: M[RequestM[A]])(implicit cbf: scala.collection.generic.CanBuildFrom[M[RequestM[A]], A, M[A]]): RequestM[M[A]] = {
    seq.foldLeft(RequestM.successful(cbf(seq))) {
      (fr, fa) => for (r <- fr; a <- fa) yield (r += a)
    } map (_.result())
  }
  
  def doReceive = {
    case FetchApps => {
/*
      val appOIDs = AppsPersistence.lookupApps(spaceId)
      
      // TODO: this is arbitrary. Probably make it configurable? Keep in mind that this may kick
      // off recursive loads.
      implicit val timeout = Timeout(1 minute)
      
      val theSender = sender
      val reqs = for {
        appId <- appOIDs
        loader = context.actorOf(AppLoader.props(ecology, appId, ownerIdentity))
        req = loader.requestFor[CurrentState](AppLoader.FetchApp)
      }
        yield req
        
      // TODO: this should become RequestM.sequence():
      val seqReq = sequenceReqs(reqs)
        
      seqReq foreach { states =>
        theSender ! AppStates(states map { _.state })
        context.stop(self)
      }
*/
    }
  }
}

private [apps] object AppLoadingActor {
  case object FetchApps
  case class AppStates(states:Seq[SpaceState])
  def props(ecology:Ecology, spaceId:OID, ownerIdentity:OID) = Props(classOf[AppLoadingActor], ecology, spaceId, ownerIdentity)
}

/**
 * Worker that fetches the State of a specific App.
 */
/*
private [apps] class AppLoader(ecology:Ecology, appId:OID, ownerIdentity:OID) extends QuerkiActor(ecology) {
  import AppLoader._
  import AppSender._
  
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  // Probably ought to be configurable?
  val retries = 3
  
  def loadChunksRec(appSender:ActorRef, curState:SpaceState, isComplete:Boolean, nextSeq:Int):RequestM[SpaceState] = {
    if (isComplete)
      RequestM.successful(curState)
    else {
      appSender.requestFor[AppChunk](RequestChunk(nextSeq), retries) flatMap { chunk =>
        val state = (curState /: chunk.elems) { (s, t) =>
          t match {
            case p:Property[_,_] => s.copy(spaceProps = s.spaceProps + (t.id -> p))
            case ts:ThingState => s.copy(things = s.things + (ts.id -> ts))
            case _ => throw new Exception(s"AppLoader somehow got unexpected Thing $t")
          }
        }
        
        loadChunksRec(appSender, state, chunk.complete, nextSeq + 1)
      }
    }
  }
  
  def loadChunks(appSender:ActorRef):RequestM[SpaceState] = {
    val firstChunkReq = appSender.requestFor[AppChunk](RequestChunk(0), retries)
    firstChunkReq flatMap { stateChunk =>
      // Yes, we're assuming the first chunk is the SpaceState. For now, that's part of the protocol:
      val initState = stateChunk.elems.head.asInstanceOf[SpaceState]
      loadChunksRec(appSender, initState, stateChunk.complete, 1)
    }
  }
  
  def doReceive = {
    case FetchApp => {
      for {
        AppStreamReady(appSender) <- SpaceOps.spaceRegion.request(SpacePluginMsg(User.Anonymous, appId, FetchAppState(ownerIdentity)), retries)
        state <- loadChunks(appSender)
      }
      {
        // Populate all of the caches for this App:
        val CacheUpdate(_, _, withCache) = SpaceChangeManager.updateStateCache(CacheUpdate(None, None, state))
        appSender ! StreamComplete
        sender ! CurrentState(withCache)
        context.stop(self)
      }
    }
  }
}

private [apps] object AppLoader {
  case object FetchApp
  def props(ecology:Ecology, appId:OID, ownerIdentity:OID) = Props(classOf[AppLoader], ecology, appId, ownerIdentity)
}
*/