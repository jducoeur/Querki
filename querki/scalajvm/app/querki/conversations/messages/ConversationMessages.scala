package querki.conversations.messages

import models.{OID, ThingId}

import querki.conversations._
import querki.identity.User
import querki.spaces.messages.SpaceMessagePayload

/**
 * Request from the Space to the Conversation system, to find out how many Things are currently loaded
 * in Conversations.
 */
case object GetActiveThings

/**
 * Response to GetActiveThings -- for now, simply the number of Things that are loaded with Conversations.
 */
case class ActiveThings(n: Int)

/**
 * Note that ConversationMessages are never sent directly on their own; instead, they are wrapped in a
 * ConversationRequest, and routed through the Space. The subclasses of ConversationMessage are the
 * payloads of ConversationRequest.
 */
sealed trait ConversationMessage extends SpaceMessagePayload

/**
 * This should get a ThingConversations as its response.
 */
case class GetConversations(thing: OID) extends ConversationMessage

/**
 * Someone has submitted a new Comment for this Space. This should get an AddedNode in response.
 */
case class NewComment(comment: Comment) extends ConversationMessage

/**
 * Message informing us about a newly-added Comment.
 */
case class AddedNode(
  parentId: Option[CommentId],
  node: ConversationNode
)

/**
 * Request to delete an existing Comment.
 */
case class DeleteComment(
  thingId: OID,
  commentId: CommentId
) extends ConversationMessage
case object CommentDeleted
case object CommentNotDeleted

/**
 * This message is internal to the Conversations system; it is used to slam the conversations for the
 * target Thing. It is only being used during the upgrade process from MySQL to Cassandra, and should
 * go away once that is completely done. Note that it is intentionally impossible to send this from
 * outside the Conversations system.
 */
case class SetConversations(convs: ThingConversations)
case class ConvsSet()
