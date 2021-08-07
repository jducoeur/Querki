package querki.conversations

import messages._

import scala.concurrent.Future

import querki.data.TID

trait ConversationFunctions {

  /**
   * Fetch the Conversations for this Thing. Note that this may simply declare that you
   * don't have the right to read these Conversations.
   */
  def getConversationsFor(thingId: TID): Future[ConversationInfo]

  /**
   * Add a Comment, to an existing Conversation or starting a new one.
   */
  def addComment(
    thingId: TID,
    text: String,
    responseTo: Option[CommentId]
  ): Future[ConvNode]

  /**
   * Delete the specified Comment.
   */
  def deleteComment(
    thingId: TID,
    commentId: CommentId
  ): Future[Unit]
}
