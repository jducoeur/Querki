package querki.conversations

import akka.actor._

import querki.conversations.messages._
import querki.globals._
import querki.identity.{IdentityId, User}
import querki.spaces.messages._
import querki.uservalues.PersistMessages._
import querki.util.QuerkiActor

import PersistMessages._

/**
 * This Actor belongs to the SpaceConversations troupe; it is responsible for sending notifications about comments
 * within this Space.
 * 
 * For the moment, this still operates in the old MySQL fashion, based on old-style User Values. This will want to
 * shift to the newer Cassandra model in the not too distant future. We might change the User Value to this simply
 * being a PersistentActor that tracks who wants what notifications in this Space, but that remains to be seen.
 * 
 * Note that this initially has a small race condition: if someone comments before the UserValue load completes, no
 * notifications will be sent. I'm intentionally not worrying about that, because I expect essentially all of this
 * code to be dumped before terribly long.
 */
private [conversations] class SpaceConversationNotifier(e:Ecology, initState:SpaceState, spaceRouter:ActorRef) extends QuerkiActor(e) {
  import SpaceConversationNotifier._

  lazy val NotifyComments = interface[NotifyComments]
  lazy val persistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val persister = persistenceFactory.getConversationPersister(initState.id)
  
  var state = initState
  var commentNotifyPrefs:Seq[OneUserValue] = Seq.empty
   
  override def preStart() = {
    for {
      ValuesForUser(prefs) <- spaceRouter.request(SpaceSubsystemRequest(User.Anonymous, state.id, LoadAllPropValues(NotifyComments.GetCommentNotesPref, state)))
    }
    {
      commentNotifyPrefs = prefs
    }
  }
  
  def doReceive = {
    case CurrentState(current, _) => {
      state = current
    }
    
    case NotifyNewComment(req, comment, parentAuthors) => {
      // TODO: this is notably primitive at the moment. In the medium term, you should also get notifications for
      // a thread if you are one of the parentAuthors.
      NotifyComments.notifyComment(req, comment, commentNotifyPrefs)(state)
    }
  }
}

object SpaceConversationNotifier {
  def actorProps(e:Ecology, initState:SpaceState, spaceRouter:ActorRef) = Props(classOf[SpaceConversationNotifier], e, initState, spaceRouter)
  
  /**
   * Sent by the ThingConversationsActor when a comment is added.
   */
  case class NotifyNewComment(req:User, comment:Comment, parentAuthors:Seq[IdentityId])
}
