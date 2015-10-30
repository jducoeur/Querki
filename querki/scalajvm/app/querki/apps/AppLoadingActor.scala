package querki.apps

import scala.concurrent.duration._

import akka.actor._
import akka.util.Timeout

import org.querki.requester._

import querki.globals._
import querki.identity.User
import querki.spaces.messages._

/**
 * The manager for loading a Space's Apps. For the time being, this is a (somewhat complex) one-shot worker.
 * 
 * This will probably get largely obviated by Akka Streams, but if not, think about switching it to Typed Akka
 * down the road.
 * 
 * TODO: there seems to be an abstraction of a Requester Worker fighting to break out here: a concise way to create
 * a Requester Actor that performs some async tasks, returns a value, and then stops.
 * 
 * @author jducoeur
 */
private [apps] class AppLoadingActor(val ecology:Ecology, spaceId:OID, ownerIdentity:OID) extends Actor with Requester with EcologyMember {
  import AppLoadingActor._
  
  lazy val AppsPersistence = interface[AppsPersistence]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  // Adapted from the similar function in Future:
  def sequenceReqs[A, M[X] <: TraversableOnce[X]](seq: M[RequestM[A]])(implicit cbf: scala.collection.generic.CanBuildFrom[M[RequestM[A]], A, M[A]]): RequestM[M[A]] = {
    seq.foldLeft(RequestM.successful(cbf(seq))) {
      (fr, fa) => for (r <- fr; a <- fa) yield (r += a)
    } map (_.result())
  }
  
  def receive = {
    case FetchApps => {
      val appOIDs = AppsPersistence.lookupApps(spaceId)
      
      // TODO: this is arbitrary. Probably make it configurable? Keep in mind that this may kick
      // off recursive loads.
      implicit val timeout = Timeout(1 minute)
      
      val theSender = sender
      val reqs = for {
        appId <- appOIDs
        req = SpaceOps.spaceRegion.requestFor[CurrentState](SpacePluginMsg(User.Anonymous, appId, FetchAppState(ownerIdentity)))
      }
        yield req
        
      // TODO: this should become RequestM.sequence():
      val seqReq = sequenceReqs(reqs)
        
      seqReq foreach { states =>
        theSender ! AppStates(states map { _.state })
        context.stop(self)
      }
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
private [apps] class AppLoader(val ecology:Ecology, appId:OID, ownerIdentity:OID) extends Actor with Requester with EcologyMember {
  import AppLoader._
  
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  def receive = {
    case FetchApp => {
      for {
        CurrentState(state) <- SpaceOps.spaceRegion ? SpacePluginMsg(User.Anonymous, appId, FetchAppState(ownerIdentity))
      }
      {
        sender ! CurrentState(state)
        context.stop(self)
      }
    }
  }
}

private [apps] object AppLoader {
  case object FetchApp
  def props(ecology:Ecology, appId:OID, ownerIdentity:OID) = Props(classOf[AppLoader], ecology, appId, ownerIdentity)
}
