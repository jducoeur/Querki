package querki.conversations

import akka.actor.Actor.Receive
import akka.persistence._

import querki.conversations.messages._
import querki.globals._
import querki.identity.{IdentityId, IdentityPersistence, User}
import querki.persistence.PersistentActorCore
import querki.spaces.messages._
import querki.time.DateTime

import PersistentEvents._

/**
 * This class is the heart of the ThingConversationsActor. It is separated out to make it 
 * more easily unit-testable.
 * 
 * There is *potentially* one of these for every Thing in Querki! However, in practice we
 * only record anything in the journal if somebody actually comments, so there are actually
 * only a tiny fraction as many as that.
 * 
 * This Actor does not stand on its own: it is a child of the SpaceConversationsManager, which
 * fires it up as necessary.
 * 
 * Note that this takes the initial State as a constructor parameter. This greatly simplifies the code.
 * 
 * For the moment, this doesn't use snapshots, on the theory that it is rare for a Thing to have
 * so much conversation as to make them useful. We might add them later: the snapshot would just
 * consist of the ThingConversations and nextId.
 */
abstract class ThingConversationsCore(initState:SpaceState, val thingId:OID)(implicit val ecology:Ecology) 
  extends EcologyMember with PersistentActorCore with PersistentEvents with IdentityPersistence
{ self:querki.types.ModelTypeDefiner =>
  
  lazy val ConvEcot = interface[Conversations]
  
  def convTrace = ConvEcot.convTrace _
  
  def notifyNewComment(req:User, comment:Comment, parentAuthors:Seq[IdentityId])
  
  implicit var state:SpaceState = initState
  val persistenceId = s"conv-${initState.id.toString}-${thingId.toString}"
  
  var conversations:ThingConversations = ThingConversations(Seq.empty)
  def updateConversations(f:ThingConversations => ThingConversations):Unit = {
    conversations = f(conversations)
  }
  def replaceAndUpdate(commentId:CommentId)(f:ConversationNode => ConversationNode):Unit = {
    conversations = replaceNode(commentId, conversations)(f)
  }
  
  /**
   * The next CommentId to assign to a newly-created Comment.
   */
  var nextId:CommentId = 1
  
  /**
   * When the outside world tells us to set the conversations, deal with side-effects.
   */
  def setConvs(convs:ThingConversations) = {
    conversations = convs
    
    def fixNextIdRec(convList:Seq[ConversationNode]):Unit = {
      convList.foreach { node =>
        if (node.comment.id >= nextId) {
          nextId = node.comment.id + 1
        }
        
        fixNextIdRec(node.responses)
      }
    }
  
    fixNextIdRec(conversations.comments)
  }
  
  /**
   * Finds and mutates the specified node, and returns the resulting tree.
   * 
   * TODO: this is too rigid to make really nasty topological changes, sadly. Is there a better way to do this?
   * I suspect I'm reinventing some category-theory wheel here.
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
  
  /**
   * The guts of adding a new comment to this Thing's Conversations.
   */
  def doAdd(comment:Comment):(Option[CommentId], ConversationNode, List[ConversationNode]) = {
    // Insert the comment into the Conversations list in the appropriate place. This
    // may involve tweaking the comment a bit in case of race conditions.
    comment.responseTo.flatMap(conversations.findNode(_)) match {
      
      // The comment is being inserted into an existing Conversation
      case Some((rawParentNode, parents)) => {
        // Find the actual parent node to insert this under, and amend the comment if
        // necessary:
        val (commentWithCorrectedParent, actualParent) = {
          if (!comment.primaryResponse || rawParentNode.responses.isEmpty)
            // Either way, this comment is simply being inserted directly under the parent. This
            // is the common case:
            (comment, rawParentNode)
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
            val actualParent = currentConvEnd(rawParentNode)
            (comment.copy(responseTo = Some(currentConvEnd(rawParentNode).comment.id)), actualParent)
          }
        }

        // Okay, now we know what the correct parent is, so modify the tree to insert it:
        val node = ConversationNode(commentWithCorrectedParent)
        val parentId = commentWithCorrectedParent.responseTo.get
        replaceAndUpdate(parentId) { parentNode =>
          parentNode.copy(responses = parentNode.responses :+ node)
        }
        (Some(parentId), node, actualParent :: parents)
      }
      
      // The comment is starting a new Conversation
      case None => {
        val node = ConversationNode(comment)
        updateConversations(convs => convs.copy(comments = convs.comments :+ node))
        (None, node, List.empty)
      }
    }
  }
  
  def doDelete(commentId:CommentId):Unit = {
    replaceAndUpdate(commentId) { node => 
      node.copy(comment = node.comment.copy(isDeleted = true))
    }
  }
  
  def receiveRecover:Receive = {
    case SnapshotOffer(metadata, msg) => {
      // We aren't yet using snapshots, so this is weird:
      QLog.error(s"ThingConversationsCore got offered a snapshot: $msg")
    }
    
    case evt:DHConvs => {
      val convs = rehydrate(evt)
      setConvs(convs)
    }
    
    case DHAddComment(comment) => {
      val (parentIdOpt, node, parents) = doAdd(rehydrate(comment))
      val commentId = node.comment.id
      if (commentId >= nextId) {
        nextId = commentId + 1
      }
    }
    
    case DHDeleteComment(req, commentId) => {
      doDelete(commentId)
    }
    
    case RecoveryCompleted => {
      // Do we need to do anything at all? I don't think so.
    }
  }
  
  def receiveCommand:Receive = {
    /**
     * Update from the Space Actor that the state has been changed.
     */
    case CurrentState(current) => {
      state = current
    }
        
    /**
     * This doesn't get a security check, because it's a system message, and *should* be impossible to
     * get here incorrectly.
     */
    case SetConversations(convs) => {
      val evt = dh(convs)
      doPersist(evt) { _ =>
        setConvs(convs)
        respond(ConvsSet())
      }
    }
    
    /**
     * This is a wrapped message that was sent to the troupe. The guts are in msg.
     */
    case SpaceSubsystemRequest(req, _, msg) => {
      msg match {
        case GetConversations(_) => {
  	      if (!ConvEcot.canReadComments(req, thingId, state)) {
  	        // You aren't allowed to read the Conversations about a Thing unless you are allowed to read the Thing:
  	        respond(ThingError(new PublicException(SpaceError.UnknownID)))
  	      } else {
  	        // TODO: if the requester is not a Moderator, strip out needsModeration comments.
  	        // TODO: strip out isDeleted comments. Does this include child comments? Probably not.
  	        respond(conversations)
  	      }          
        }
        
        /**
         * Requester has sent a new Comment to be appended.
         */
        case NewComment(commentIn) => {
          convTrace(s"    Conv Actor got a NewComment for $commentIn")
  	      if (!ConvEcot.canWriteComments(commentIn.authorId, thingId, state)) {
  	        // TODO: if Moderation is enabled for comments on this Thing, add it with needsModeration turned on, and
  	        // send a Notification to the moderator(s), instead of rejecting it outright like this:
  	        respond(ThingError(new PublicException(SpaceError.ModifyNotAllowed)))
  	      } else {
            val comment = commentIn.copy(id = nextId, createTime = DateTime.now)
            nextId += 1
            val evt = DHAddComment(dh(comment))
            doPersist(evt) { _ =>
              val (parent, node, parents) = doAdd(comment)
              
              // Send the ack of the newly-created comment, saying where to place it:
              respond(AddedNode(parent, node))
              
              // Send out Notifications -- fire-and-forget, will get there eventually:
              val threadAuthors = parents.map(_.comment.authorId)
              notifyNewComment(req, node.comment, threadAuthors)
            }
  	      }
        }
        
        // TODO: this needs unit-testing, before we get around to actually implementing the UI:
        case DeleteComment(_, commentId) => {
          conversations.findNode(commentId) match {
            case Some((node, parents)) => {
              if (req.hasIdentity(state.owner) || req.hasIdentity(node.comment.authorId)) {
                doPersist(DHDeleteComment(req, commentId)) { _ =>
                  doDelete(commentId)
                  respond(CommentDeleted)
                  // TODO: we really should delete the notifications, but we have no mechanism for doing so
                  // currently. Hmm...
                }
              } else {
                respond(CommentNotDeleted)
              }
            }
            case None => respond(CommentNotDeleted)
          }
        }
      }
    }
  }
}
