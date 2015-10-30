package querki.apps

import scala.concurrent.duration._

import akka.actor._
import akka.util.Timeout

import org.querki.requester._

import querki.globals._
import querki.identity.User
import querki.spaces.messages._

/**
 * The manager for loading a Space's Apps.
 * 
 * @author jducoeur
 */
private [apps] class AppLoadingActor(val ecology:Ecology, spaceId:OID, ownerIdentity:OID) extends Actor with Requester with EcologyMember {
  import AppLoadingActor._
  
  lazy val AppsPersistence = interface[AppsPersistence]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  def receive = {
    case FetchApps => {
      val appOIDs = AppsPersistence.lookupApps(spaceId)
      
      // TODO: this is arbitrary. Probably make it configurable? Keep in mind that this may kick
      // off recursive loads.
      implicit val timeout = Timeout(1 minute)
      
      // This all needs rewriting, to be Requester-based and split into distinct loaders:
      val theSender = sender
      val futs = for {
        appId <- appOIDs
        askFut = (SpaceOps.spaceRegion ? SpacePluginMsg(User.Anonymous, appId, FetchAppState(ownerIdentity))).mapTo[CurrentState]
      }
        yield askFut map (_.state)
        
      Future.sequence(futs) foreach { states =>
        theSender ! AppStates(states)
      }
    }
  }
}

private [apps] object AppLoadingActor {
  case object FetchApps
  case class AppStates(states:Seq[SpaceState])
  def props(ecology:Ecology, spaceId:OID, ownerIdentity:OID) = Props(classOf[AppLoadingActor], ecology, spaceId, ownerIdentity)
}
