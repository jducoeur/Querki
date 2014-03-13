package querki

import akka.actor.{ActorRef, Props}

import models.OID

import querki.ecology._
import querki.spaces.SpacePersistenceFactory

package object conversations {
  /**
   * The main public interface for Conversations.
   * 
   * Note that, since Conversations are highly stateful, most work with them go through Actors, *not*
   * through the Ecology. There is one Actor per Space which is in charge of managing that Space's Conversations.
   */
  trait Conversations extends EcologyInterface {
    /**
     * The public interface to create a SpaceConversationsActor.
     */
    def conversationActorProps(persistenceFactory:SpacePersistenceFactory, spaceId:OID, space:ActorRef):Props
    
    /**
     * The interface to build the Persister for a Space's Conversations.
     */
    def conversationPersisterProps(spaceId:OID):Props
  }
}