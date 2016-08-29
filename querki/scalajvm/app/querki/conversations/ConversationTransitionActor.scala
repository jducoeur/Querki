package querki.conversations

import akka.actor._

import org.querki.requester._

import querki.conversations.messages._
import querki.globals._
import querki.spaces.SpacePersistenceFactory
import querki.time.{DateTime, DateTimeOrdering}
import querki.util.QuerkiActor

/**
 * This is a temporary Actor, whose sole purpose is to port Conversations from the old
 * MySQL world to the new Akka Persistence one. Once we are reasonably sure that all Spaces
 * have been ported, this should be able to go away.
 */
class ConversationTransitionActor(e:Ecology, state:SpaceState, router:ActorRef, persistenceFactory:SpacePersistenceFactory) extends QuerkiActor(e) {
  import ConversationTransitionActor._
  import PersistMessages._
  
  lazy val persister = persistenceFactory.getConversationPersister(state.id)
  
  /**
   * Given a bunch of Comments, stitch them together into Conversations.
   * 
   * This code is lifted directly from the old SpaceConversationsActor, which is now dead.
   */
  def buildConversations(comments:Seq[Comment]):ThingConversations = {
    val (dependencies, roots) = ((Map.empty[CommentId, Seq[Comment]], Seq.empty[Comment]) /: comments) { (info, comment) =>
      val (dep, roots) = info
      comment.responseTo match {
        case Some(parentId) => {
          dep.get(parentId) match {
            case Some(children) => (dep + (parentId -> (children :+ comment)), roots)
            case None => (dep + (parentId -> Seq(comment)), roots)
          }
        }
        case None => (dep, roots :+ comment)
      }
    }
    
    def buildNodes(branches:Seq[Comment]):Seq[ConversationNode] = {
      val nodes = (Seq.empty[ConversationNode] /: branches) { (seq, branch) =>
        val children = dependencies.get(branch.id).map(buildNodes(_)).getOrElse(Seq.empty[ConversationNode])
        seq :+ ConversationNode(branch, children)
      }
      nodes.sortBy(node => node.comment.createTime)
    }
    
    ThingConversations(buildNodes(roots))
  }
  
  def doReceive = {
    case RunTransition => {
      for {
        allComments <- persister.requestFor[AllComments](LoadAllComments(state))
        commentsByThing = allComments.comments.groupBy(_.thingId)
        convsByThing = commentsByThing.map { case (thingId, comments) => (thingId, buildConversations(comments)) }
        // Go through all the Things that have conversations. For each, fire up a ThingConversationsActor, send it
        // the conversations, and wait for an Ack. This is a bit high-latency, but avoids saturating anything.
        _ <- (RequestM.successful(ConvsSet()) /: convsByThing) { case (prevReq, (thingId, convs)) =>
          prevReq.flatMap { _ =>
            val convActor = context.actorOf(ThingConversationsActor.actorProps(state, thingId, ecology))
            convActor.requestFor[ConvsSet](SetConversations(convs)).map { set =>
              // After the Actor acknowledges that it is done, shut it down:
              context.stop(convActor)
              set
            }
          }
        }
      }
        sender ! TransitionComplete
    }
  }
}

object ConversationTransitionActor {
  def actorProps(e:Ecology, state:SpaceState, router:ActorRef, persistenceFactory:SpacePersistenceFactory) =
    Props(classOf[ConversationTransitionActor], e, state, router, persistenceFactory)
  
  case object RunTransition
  case object TransitionComplete
}
