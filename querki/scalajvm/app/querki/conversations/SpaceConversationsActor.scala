package querki.conversations

import akka.actor._

import models.OID

import org.querki.requester._

import querki.conversations.messages._
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.spaces.SpacePersistenceFactory
import querki.spaces.messages._
import querki.time.{DateTime, DateTimeOrdering}
import querki.uservalues.PersistMessages._
import querki.util.{PublicException, QuerkiBootableActor}
import querki.values.SpaceState

import PersistMessages._

/**
 * The SpaceConversationActor manages the Conversations for one specific Space. Its lifespan is basically the
 * same as the Space itself -- it is created when the Space is, and shuts down when the Space is unloaded.
 * Unlike the Space, though, it does not keep all the Conversations in memory all the time: it loads and unloads
 * them as needed.
 * 
 * Note that this Actor is an especially good candidate to become an EventSourcedProcessor. We really should rework
 * it at some point, replacing the persister with Akka Persistence.
 * 
 * IMPORTANT: this implies that all changes should happen in EventSourced style, to make the evolution easier when it
 * happens. We should receive a Command, validate that Command, and then persist an Event that makes the actual change
 * to the state.
 * 
 * TODO: is this code dead? I think this code is dead, replaced by ThingConversationsCore.
 */
private [conversations] class SpaceConversationsActor(ecology:Ecology, persistenceFactory:SpacePersistenceFactory, val spaceId:OID, val space:ActorRef)
  extends QuerkiBootableActor(ecology)
{
  import context._
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ConvEcot = interface[Conversations]
  lazy val NotifyComments = interface[NotifyComments]
  
  def convTrace = ConvEcot.convTrace _
  
  lazy val persister = persistenceFactory.getConversationPersister(spaceId)
  
  /**
   * The Conversations that are currently loaded into memory.
   * 
   * TODO: these should time out after a few minutes of disuse. Maybe a PriorityQueue? Or should we have a sub-Actor for
   * each Thing, which would be more compatible with evolving to EventSourcedProcessor?
   */
  var loadedConversations:Map[OID, ThingConversations] = Map.empty
  
  var commentNotifyPrefs:Seq[OneUserValue] = Seq.empty
  
  /**
   * IMPORTANT: this must be set before we begin any serious work! This is why we start
   * in a rudimentary state, and don't become useful until it is received.
   */
  var state:SpaceState = null
  
  /**
   * The next CommentId to assign to a newly-created Comment.
   */
  var nextId:CommentId = 1
  
  def bootReceive = {
    /**
     * This Actor can't become properly active until we receive the current state to work with:
     */
    case CurrentState(current, _) => {
      // Only go through boot if this is the first time we get the state.
      val boot = (state == null)
      
      state = current
      
      if (boot) {
        for {
          CurrentMaxCommentId(n) <- persister.request(GetMaxCommentId)
          ValuesForUser(prefs) <- space.request(SpaceSubsystemRequest(User.Anonymous, state.id, LoadAllPropValues(NotifyComments.GetCommentNotesPref, state)))
        }
        {
          nextId = n + 1
          commentNotifyPrefs = prefs
          doneBooting()
        }
      }
    }
  }
  
  /**
   * Given a bunch of Comments, stitch them together into Conversations.
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
  
  /**
   * Core abstraction for "do something with the Conversations for this Thing". Ensures that
   * the conversations are loaded into memory, then runs the given function on them.
   * 
   * IMPORTANT: this is asynchronous! Assume that f() may be run either immediately or at some
   * time in the future. If it is in the future, it will be run by Requester, so it may access
   * the Actor state arbitrarily, but be careful about any local data you close over!
   */
  def withConversations(thingId:OID)(f:ThingConversations => Unit) = {
    loadedConversations.get(thingId) match {
      case Some(convs) => f(convs)
      case None => {
        persister.request(LoadCommentsFor(thingId, state)) foreach {
          case AllCommentsFor(_, comments) => {
            // Race condition check: some other request might have loaded this Thing's conversations while
            // we were in the roundtrip. In that case, the already-existing copy is authoritative, because it
            // might have mutated. (In other words, don't keep chasing the race round and round.)
            val convs = loadedConversations.get(thingId) match {
              case Some(newCs) => newCs
              case None => {
                val cs = buildConversations(comments)
                loadedConversations += (thingId -> cs)
                cs
              }
            }
            f(convs)
          }
        }
      }
    }    
  }
  
  /**
   * Finds and mutates the specified node, and returns the resulting tree.
   * 
   * TODO: this is too rigid to make really nasty topological changes, sadly. Is there a better way to do this?
   */
  def replaceNode(commentId:CommentId, convs:ThingConversations)(f:ConversationNode => ConversationNode):ThingConversations = {
    
    def replaceChildNodes(children:Seq[ConversationNode]):(Seq[ConversationNode], Option[ConversationNode], Seq[ConversationNode]) = {
      ((Seq.empty[ConversationNode], Option.empty[ConversationNode], Seq.empty[ConversationNode]) /: children) { (state, child) =>
        val (sPre, found, sPost) = state
        found match {
          // We're after the match
          case Some(f) => (sPre, found, sPost :+ child)
          case None => {
            replaceNodeRec(child) match {
              // This one's the match
              case Some(changed) => (sPre, Some(changed), sPost)
              // Haven't found the match yet:
              case None => (sPre :+ child, None, sPost)
            }
          }
        }
      }      
    }
    
    def replaceNodeRec(node:ConversationNode):Option[ConversationNode] = {
      if (node.comment.id == commentId) {
        Some(f(node))
      } else {
        // Go through my direct children; if one of them contains the result, restitch things together
        val (pre, theOne, post) = replaceChildNodes(node.responses)
        
        theOne match {
          case Some(changedChild) => {
            // Okay, a child has changed, so I need to change:
            Some(node.copy(responses = (pre :+ changedChild) ++ post))
          }
          case None => None
        }
      }
    }
    
    val (pre, theOne, post) = replaceChildNodes(convs.comments)
    // TODO: iff theOne is empty, it means something weird has happened -- we didn't find the specified parent.
    // Should we report an error?
    convs.copy(comments = pre ++ theOne ++ post)
  }
  
  def doReceive:Receive = {
    /**
     * Update from the Space Actor that the state has been changed.
     */
    case CurrentState(current, _) => {
      state = current
    }
    
    case GetActiveThings => sender ! ActiveThings(loadedConversations.size)
    
    case SpaceSubsystemRequest(req, _, msg) => {
      msg match {
      /**
       * Requester is fetching the current Conversations for this Thing.
       */
        case GetConversations(thingId) => {
          if (!ConvEcot.canReadComments(req, thingId, state)) {
            // You aren't allowed to read the Conversations about a Thing unless you are allowed to read the Thing:
            sender ! ThingError(new PublicException(SpaceError.UnknownID))
          } else {
            // TODO: if the requester is not a Moderator, strip out needsModeration comments.
            // TODO: strip out isDeleted comments.
            withConversations(thingId) { convs => sender ! convs }
          }           
        }
        
        /**
         * Requester has sent a new Comment to be appended.
         * 
         * TODO: if we're going to work correctly with Akka Persistence, we need to cope with occasional duplicate
         * NewComment messages. So the protocol ought to have some sort of ID that is generated by the sender, which
         * we can use to detect duplication. See http://www-db.cs.wisc.edu/cidr/cidr2007/papers/cidr07p15.pdf
         */
        case NewComment(commentIn) => {
          convTrace(s"    Conv Actor got a NewComment for $commentIn")
          val comment = commentIn.copy(id = nextId, createTime = DateTime.now)
          nextId += 1
          val thingId:OID = comment.thingId
          if (!ConvEcot.canWriteComments(comment.authorId, thingId, state)) {
            // TODO: if Moderation is enabled for comments on this Thing, add it with needsModeration turned on, and
            // send a Notification to the moderator(s), instead of rejecting it outright like this:
            sender ! ThingError(new PublicException(SpaceError.ModifyNotAllowed))
          } else {
            convTrace(s"    Fetching the conversations, to add the comment")
            // Fetch the Conversations for this Thing. Note that the innards here may be async!
            withConversations(thingId) { convs =>
              convTrace(s"    Have the conversations")
              // Insert the comment into the Conversations list in the appropriate place. This
              // may involve tweaking the comment a bit in case of race conditions.
              val (parent, node) = comment.responseTo.flatMap(convs.findNode(_)) match {
                
                // The comment is being inserted into an existing Conversation
                case Some((rawParentNode, parents)) => {
                  // Find the actual parent node to insert this under, and amend the comment if
                  // necessary:
                  val commentWithCorrectedParent = {
                    if (!comment.primaryResponse || rawParentNode.responses.isEmpty)
                      // Either way, this comment is simply being inserted directly under the parent. This
                      // is the common case:
                      comment
                    else {
                      // Race condition: there is already a "primary" response, and this is also trying to
                      // be one. This can happen if multiple people both reply directly to the comment, and
                      // don't see each other's new comment. Tack this new one onto the
                      // end of the conversation instead. We find that by recursing down the primary
                      // responses:
                      def currentConvEnd(parentNode:ConversationNode):ConversationNode = {
                        parentNode.responses.find(_.comment.primaryResponse) match {
                          case Some(primaryChild) => currentConvEnd(primaryChild)
                          case None => parentNode
                        }
                      }
                      comment.copy(responseTo = Some(currentConvEnd(rawParentNode).comment.id))
                    }
                  }
  
                  // Okay, now we know what the correct parent is, so modify the tree to insert it:
                  val node = ConversationNode(commentWithCorrectedParent)
                  var pNode:Option[ConversationNode] = None
                  val newConvs = replaceNode(commentWithCorrectedParent.responseTo.get, convs) { parentNode =>
                    pNode = Some(parentNode)
                    parentNode.copy(responses = parentNode.responses :+ node)
                  }
                  loadedConversations += (thingId -> newConvs)
                  (pNode, node)
                }
                
                // The comment is starting a new Conversation
                case None => {
                  val node = ConversationNode(comment)
                  loadedConversations += (thingId -> convs.copy(comments = convs.comments :+ node))
                  (None, node)
                }
              }
              
              // Okay, we have now placed the comment in the tree. Persist it. This is simply
              // fire-and-forget:
              persister ! AddComment(node.comment, state)
              
              // Send the ack of the newly-created comment, saying where to place it:
              sender ! AddedNode(parent.map(_.comment.id), node)
              
              // Finally, send out Notifications -- fire-and-forget, will get there eventually:
              NotifyComments.notifyComment(req, comment, commentNotifyPrefs)(state)
            }
          }
        }
        
        case DeleteComment(thingId, commentId) => {
         withConversations(thingId) { convs =>
            convs.findNode(commentId) match {
              case Some((node, parents)) => {
                 // Only Moderators and the original Author of the Comment are allowed to delete it:
                if (AccessControl.hasPermission(ConvEcot.CanModerate, state, req, thingId) || req.hasIdentity(node.comment.authorId)) {
                  // Create the tweaked node...
                  val deleted = node.comment.copy(isDeleted = true)
                  // ... create an updated ThingConversations with it...
                  val newConvs = replaceNode(commentId, convs)(_.copy(comment = deleted))
                  // ... update the world...
                  loadedConversations += (thingId -> newConvs)
                  // ... persist the deletion (this is fire-and-forget)...
                  persister ! UpdateComment(deleted, state)
                  // ... and tell the requester we are done:
                  sender ! CommentDeleted
                  
                  // TODO: we really should delete the notifications, but we have no mechanism for doing so
                  // currently. Hmm...
                } else {
                  sender ! CommentNotDeleted
                }
              }
              case None => sender ! CommentNotDeleted
            }
          }
        }
      }
    }
  }
}
