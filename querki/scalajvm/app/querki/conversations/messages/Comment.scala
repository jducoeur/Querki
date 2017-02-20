package querki.conversations.messages

import models.{OID, PropMap}
import querki.identity.IdentityId
import querki.time.{DateTime, epoch}

/**
 * The public representation of a single Comment. Nominally part of the package interface, but Eclipse complains.
 */
case class Comment(
    /**
     * The OID of the Space that this Comment is contained in.
     */
    spaceId:OID,
    /**
     * The ID of this Comment, which is unique within this Space. 
     */
    id:CommentId,
    /**
     * The OID of the Thing this Comment is associated with.
     */ 
    thingId:OID,
    /**
     * The OID of the *Identity* who wrote this Comment. 
     */
    authorId:IdentityId,
    /**
     * The OID of the Moderator who authorized this Comment.
     */
    authorizedBy:Option[OID],
    /**
     * The actual contents of this Comment.
     */
    props:PropMap,
    /**
     * Which Comment this one was in response to.
     */
    responseTo:Option[CommentId],
    /**
     * True iff this Comment is nominally the "primary" one to the previous. Mainly relevant for rendering.
     */
    primaryResponse:Boolean,
    /**
     * When this Comment was created. Does *not* change if the Comment gets edited, because
     * this is mainly relevant to figure out the order to render the Conversation.
     */
    createTime:DateTime = epoch,
    /**
     * True iff this Comment is in the Moderation queue (and therefore shouldn't be displayed except to Mods).
     */
    needsModeration:Boolean = false,
    /**
     * True iff this Comment has been edited since it was created.
     */
    isEdited:Boolean = false,
    /**
     * True iff this Comment has been deleted. (In which case, this Comment usually will not be visible to user code.)
     */
    isDeleted:Boolean = false,
    /**
     * True iff this COmment has been archived. (So it shouldn't be immediately shown, but is available.)
     */
    isArchived:Boolean = false
  ) 
  
/**
 * Represents one node in a Conversation
 */
case class ConversationNode(
    /**
     * The actual Comment.
     */
    comment:Comment,
    /**
     * The direct responses to this Comment. The head of the list is considered the "primary"; the
     * rest are considered tangents.
     */
    responses:Seq[ConversationNode] = Seq.empty
  )

/**
 * Container that holds all of the Conversations for a given Thing. This is just the roots of each Conversation tree;
 * the bulk of the contents is under each root.
 */
case class ThingConversations(comments:Seq[ConversationNode]) {
  
  /**
   * Looks up a specific comment's node in the Conversations.
   * 
   * Note that we are very explicitly assuming that the number of Comments per Thing is smallish,
   * so it is easier to do a depth-first search instead of building a Map for lookups.
   * 
   * This returns both the located Node and its parent chain if there is one.
   */
  def findNode(commentId:CommentId):Option[(ConversationNode, List[ConversationNode])] = {
    def findNodeRec(nodes:Seq[ConversationNode], parents:List[ConversationNode]):Option[(ConversationNode, List[ConversationNode])] = {
      // Look -- I found the right way to deconstruct a Seq! Yay!
      nodes match {
        case node +: rest => {
          // Is this node the one we want?
          if (node.comment.id == commentId)
            Some((node, parents))
          else {
            // Try the children, and then the successors in the list:
            findNodeRec(node.responses, node :: parents) orElse findNodeRec(rest, parents)
          }
        }
        // We've hit a dead end:
        case Seq() => None
      }
    }
    
    findNodeRec(comments, List.empty)
  }
}