package querki.spaces

import akka.actor._

import models.{AsName, AsOID}

import querki.globals._
import messages.SpaceMessage
import querki.util.RoutingParent

/**
 * This is a temporary class, being refactored out of SpaceManager. It represents the bits that should
 * eventually be moved over to ClusterSharding instead. Conceptually, this is one ShardRegion.
 * 
 * @author jducoeur
 */
class SpaceRegion(val ecology:Ecology) extends Actor with EcologyMember with RoutingParent[OID]  {
  lazy val persistenceFactory = interface[SpacePersistenceFactory]
  
  /**
   * This Actor deals with all DB-style operations for the SpaceManager.
   */
  lazy val persister = persistenceFactory.getSpaceManagerPersister
  
  /**
   * TODO: take persistenceFactory out of the constructor for SpaceRouter; have it deal with that itself.
   */
  def createChild(key:OID):ActorRef = context.actorOf(SpaceRouter.actorProps(ecology, persistenceFactory, key), sid(key))

  def receive = {
    case req @ SpaceMessage(_, _, spaceId) => routeToChild(spaceId, req)   
  }
}
