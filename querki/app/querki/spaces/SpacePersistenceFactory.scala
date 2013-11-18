package querki.spaces

import akka.actor.{ActorContext, ActorRef, Props}

import models.OID

/**
 * This is the abstract factory that creates persistence Actors. It is pulled out into
 * a trait so that test harnesses can stub it with well-controlled mocks.
 */
trait SpacePersistenceFactory {
  def getSpacePersister(spaceId:OID)(implicit context:ActorContext):ActorRef
  def getSpaceManagerPersister(implicit context:ActorContext):ActorRef
}

class DBSpacePersistenceFactory extends SpacePersistenceFactory {
  def getSpacePersister(spaceId:OID)(implicit context:ActorContext):ActorRef = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    context.actorOf(Props(new SpacePersister(spaceId)), SpacePersister.sid(spaceId) + "-persist")
  }
  
  // TBD: this might actually want to be a *pool* of persistence workers. There is no real reason
  // to have only one persister for SpaceManager, and strong reason to have a hive of them. Keep
  // an eye on this, and consider turning this into a worker pool with a router in front.
  def getSpaceManagerPersister(implicit context:ActorContext):ActorRef = {
    context.actorOf(Props[SpaceManagerPersister], "space-manager-persist")    
  }
}