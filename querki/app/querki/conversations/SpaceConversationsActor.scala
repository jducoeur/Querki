package querki.conversations

import akka.actor._

import models.OID

import querki.conversations.messages._
import querki.ecology._
import querki.spaces.SpacePersistenceFactory
import querki.spaces.messages._
import querki.time.DateTimeOrdering
import querki.util.{PublicException, Requester}
import querki.values.SpaceState

import PersistMessages._

/**
 * The SpaceConversationActor manages the Conversations for one specific Space. Its lifespan is basically the
 * same as the Space itself -- it is created when the Space is, and shuts down when the Space is unloaded.
 * Unlike the Space, though, it does not keep all the Conversations in memory all the time: it loads and unloads
 * them as needed.
 */
private [conversations] class SpaceConversationsActor(val ecology:Ecology, persistenceFactory:SpacePersistenceFactory, val spaceId:OID, val space:ActorRef)
  extends Actor with Requester with EcologyMember
{
  import context._
  
  lazy val persister = persistenceFactory.getConversationPersister(spaceId)
  
  /**
   * The Conversations that are currently loaded into memory.
   * 
   * TODO: these should time out after a few minutes of disuse. Maybe a PriorityQueue?
   */
  var loadedConversations:Map[OID, ThingConversations] = Map.empty
  
  /**
   * IMPORTANT: this must be set before we begin any serious work! This is why we start
   * in a rudimentary state, and don't become useful until it is received.
   */
  var state:SpaceState = null
  
  def receive = {
    /**
     * This Actor can't become properly active until we receive the current state to work with:
     */
    case CurrentState(current) => {
      state = current
      become(normalReceive)
    }
  }
  
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
        val children = buildNodes(dependencies(branch.id))
        seq :+ ConversationNode(branch, children)
      }
      nodes.sortBy(node => node.comment.createTime)
    }
    
    ThingConversations(buildNodes(roots))
  }
  
  def normalReceive:Receive = {
    /**
     * Update from the Space Actor that the state has been changed.
     */
    case CurrentState(current) => {
      state = current
    }
    
    /**
     * Requester is fetching the current Conversations for this Thing.
     */
    case ConversationRequest(req, _, _, GetConversations(thingId)) => {
      if (!state.canRead(req, thingId)) {
        // You aren't allowed to read the Conversations about a Thing unless you are allowed to read the Thing:
        sender ! ThingError(new PublicException(SpaceError.UnknownID))
      }
      else if (loadedConversations.contains(thingId)) {
        loadedConversations.get(thingId) match {
          case Some(convs) => sender ! convs
          case None => {
            persister.request(LoadCommentsFor(thingId, state)) {
              case AllCommentsFor(_, comments) => {
                val convs = buildConversations(comments)
                loadedConversations += (thingId -> convs)
                sender ! convs
              }
            }
          }
        }
      }
        
    }
  }
}
