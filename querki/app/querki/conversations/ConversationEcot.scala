package querki.conversations

import akka.actor.{ActorRef, Props}

import querki.ecology._
import querki.identity.User
import querki.spaces.SpacePersistenceFactory
import querki.values.SpaceState

object MOIDs extends EcotIds(35) {
  
}
import MOIDs._

class ConversationEcot(e:Ecology) extends QuerkiEcot(e) with Conversations {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def conversationActorProps(persistenceFactory:SpacePersistenceFactory, spaceId:OID, space:ActorRef):Props = 
    Props(new SpaceConversationsActor(ecology, persistenceFactory, spaceId, space))
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def conversationPersisterProps(spaceId:OID):Props = 
    Props(new ConversationPersister(spaceId, ecology))
      
  def canReadComments(req:User, thingId:OID, state:SpaceState) = {
    // TODO: this will eventually need its own permission
    state.canRead(req, thingId)
  }
  
  def canWriteComments(identity:OID, thingId:OID, state:SpaceState) = {
    // TODO: this will eventually need its own permission
    AccessControl.isMember(identity, state)
  }
}