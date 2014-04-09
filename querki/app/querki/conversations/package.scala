package querki

import akka.actor.{ActorRef, Props}

import models.{OID, Property}

import querki.basic.PlainText
import querki.ecology._
import querki.identity.User
import querki.spaces.SpacePersistenceFactory
import querki.values.SpaceState

package object conversations {
  /**
   * The main public interface for Conversations.
   * 
   * Note that, since Conversations are highly stateful, most work with them go through Actors, *not*
   * through the Ecology. There is one Actor per Space which is in charge of managing that Space's Conversations.
   */
  trait Conversations extends EcologyInterface {
    
    /**
     * The actual text body of a Comment.
     */
    def CommentText:Property[PlainText, String]
    
    /**
     * Who is allowed to write Comments on this.
     */
    def CanComment:Property[OID,OID]
    
    /**
     * Who can read Comments on this.
     */
    def CanReadComments:Property[OID,OID]
    
    /**
     * The public interface to create a SpaceConversationsActor.
     */
    def conversationActorProps(persistenceFactory:SpacePersistenceFactory, spaceId:OID, space:ActorRef):Props
    
    /**
     * The interface to build the Persister for a Space's Conversations.
     */
    def conversationPersisterProps(spaceId:OID):Props
    
    /**
     * Says whether this User is allowed to read comments on this Thing.
     */
    def canReadComments(req:User, thingId:OID, state:SpaceState):Boolean
    
    /**
     * Says whether this *Identity* is allowed to write comments on this Thing. Not User -- the difference is subtle, but important,
     * since the Identity is what gets given access.
     */
    def canWriteComments(identity:OID, thingId:OID, state:SpaceState):Boolean
  }
}