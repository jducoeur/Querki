package querki.spaces

import akka.actor.{ActorContext, ActorRef, Props}
import akka.actor.SupervisorStrategy._
import akka.routing.{DefaultResizer, FromConfig, SmallestMailboxRouter}

import querki.ecology._

import models.OID

/**
 * This is the abstract factory that creates persistence Actors. It is pulled out into
 * a trait so that test harnesses can stub it with well-controlled mocks.
 */
trait SpacePersistenceFactory extends EcologyInterface {
  def getSpacePersister(spaceId:OID)(implicit context:ActorContext):ActorRef
  def getSpaceManagerPersister(implicit context:ActorContext):ActorRef
  def getConversationPersister(spaceId:OID)(implicit context:ActorContext):ActorRef
}

class DBSpacePersistenceFactory(e:Ecology) extends QuerkiEcot(e) with SpacePersistenceFactory with EcologyMember {
  
  lazy val Conversations = interface[querki.conversations.Conversations]
  
  def getSpacePersister(spaceId:OID)(implicit context:ActorContext):ActorRef = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    context.actorOf(Props(new SpacePersister(spaceId, ecology)), sid(spaceId) + "-persist")
  }

  // Note that we actually create a *pool* of persistence routers for this job, because it is important,
  // central, and relatively time-consuming.
  def getSpaceManagerPersister(implicit context:ActorContext):ActorRef = {
    // TODO: this really ought to be defined in config, but there doesn't appear to be a way in 2.1.4
    // to define the supervisorStrategy with FromConfig() yet. So for now, we do it by hand:
    context.actorOf(Props(new SpaceManagerPersister(ecology)).withRouter(
        SmallestMailboxRouter(supervisorStrategy = stoppingStrategy,
            resizer = Some(DefaultResizer(lowerBound = 2, upperBound = 10)))), 
      "space-manager-persist")
//    context.actorOf(Props[SpaceManagerPersister].withRouter(FromConfig(supervisorStrategy = stoppingStrategy)), "space-manager-persist")    
  }
  
  def getConversationPersister(spaceId:OID)(implicit context:ActorContext):ActorRef = {
    context.actorOf(Conversations.conversationPersisterProps(spaceId), "Persist")
  }
}