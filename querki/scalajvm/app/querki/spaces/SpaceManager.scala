package querki.spaces

import language.postfixOps
import scala.util._

import akka.actor._

import org.querki.requester._

import models._
import messages._

import querki.cluster.OIDAllocator._
import querki.core.NameUtils
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.spaces._
import querki.util.UnexpectedPublicException

import PersistMessages._

/**
 * TODO: all the features here probably can and should be moved out of this singleton, and be handled
 * from UserSession instead. Provided we can hook UserSession to the pool of Persisters, that ought to just
 * work.
 */
class SpaceManager(
  e: Ecology,
  val region: ActorRef
) extends Actor
     with Requester
     with EcologyMember
     with SpaceCreator {

  implicit val ecology = e

  private lazy val QuerkiCluster = interface[querki.cluster.QuerkiCluster]
  lazy val SpaceOps = interface[SpaceOps]
  lazy val persistenceFactory = interface[SpacePersistenceFactory]

  /**
   * This Actor deals with all DB-style operations for the SpaceManager.
   */
  lazy val persister = persistenceFactory.getSpaceManagerPersister

  def receive = {
    // This is entirely a DB operation, so just have the Persister deal with it:
    case req @ ListMySpaces(owner) => persister.forward(req)

//    TODO: how do we implement this in Cluster Sharding? Current theory is that we'll use
//    Distributed PubSub, with each SpaceRouter subscribing and the Admin system sending.
//    case req @ GetSpacesStatus(requester) => {
//      if (requester.isAdmin) {
//        // Each Space responds for itself:
//        children.foreach(space => space.forward(req))
//      } else {
//        QLog.error("Illegal request for GetSpacesStatus, from user " + requester.id)
//      }
//    }

    case req @ GetSpaceCount(requester) => {
      if (requester.isAdmin) {
        persister.forward(req)
      } else {
        QLog.error("Illegal request for GetSpaceCount, from user " + requester.id)
      }
    }

    case req @ CreateSpace(requester, display, initialStatus) => {
      val canon = NameUtils.canonicalize(display)
      val result = for {
        NewOID(spaceId) <- QuerkiCluster.oidAllocator.request(NextOID)
        _ <- createSpace(requester, spaceId, canon, display, initialStatus)
      } yield spaceId

      result.onComplete {
        case Failure(ex: PublicException) => sender ! ThingError(ex, None)
        case Failure(ex)                  => sender ! ThingError(UnexpectedPublicException, None)
        case Success(spaceId) => {
          // In the new Akka Persistence world, we send the new Space an InitialState message to finish
          // bootstrapping it:
          SpaceOps.spaceRegion.request(InitialState(requester, spaceId, display, requester.mainIdentity.id)).foreach {
            // *Now* the Space should be fully bootstrapped, so send the response back to the originator:
            case StateInitialized => {
              // Normally that's it, but if this is a non-Normal creation, we shut down the "real" Actor so
              // that the creator can do horrible things with a locally-created one. See for example ImportSpace.
              val req =
                if (initialStatus != StatusNormal) {
                  SpaceOps.spaceRegion.requestFor[ShutdownAck.type](ShutdownSpace(requester, spaceId))
                } else {
                  RequestM.successful(ShutdownAck)
                }
              req.map { _ =>
                sender ! SpaceInfo(spaceId, canon, display, requester.mainIdentity.handle)
              }
            }
            case ex: ThingError => sender ! ex
          }

        }
      }
    }

    case req: GetSpaceByName    => persister.forward(req)
    case req: ChangeSpaceStatus => persister.forward(req)
  }
}
