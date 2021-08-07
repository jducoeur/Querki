package querki

import scala.concurrent.Future
import scala.reflect.ClassTag

import akka.actor.ActorRef

import querki.ecology._
import querki.identity.{Identity, PublicIdentity, User}

package object session {

  /**
   * Provides access to the UserSessions.
   */
  trait Session extends EcologyInterface {

    /**
     * The root of the UserSession hierarchy. Other Actors may use this directly to send messages to UserSessions.
     */
    def sessionManager: ActorRef

    /**
     * Asynchronously fetch all of this Identity's Collaborators -- people they share Spaces with --  who fit the given search term.
     */
    def getCollaborators(
      user: User,
      identity: Identity,
      term: String
    ): Future[UserSessionMessages.Collaborators]
  }
}
