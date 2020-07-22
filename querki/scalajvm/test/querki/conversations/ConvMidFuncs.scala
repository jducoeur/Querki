package querki.conversations

import autowire._

import org.scalatest.Matchers._

import querki.conversations.messages._
import querki.data.TID
import querki.globals._
import querki.test.mid._

import AllFuncs._

/**
  * Mid-level test wrappers around the ConversationFunctions API, and other convenience functions.
  */
trait ConvMidFuncs {
  def getConversationsFor(thingId: TID): TestOp[ConversationInfo] =
    TestOp.client { _[ConversationFunctions].getConversationsFor(thingId).call() }

  /**
    * Begin a Conversation with a comment from the current user.
    *
    * @param thingId The Thing to attach this conversation to.
    * @param text The QLText to post.
    * @return The created node.
    */
  def startConversation(thingId: TID, text: String): TestOp[ConvNode] =
    TestOp.client { _[ConversationFunctions].addComment(thingId, text, None).call() }

  /**
    * Respond to a previous comment.
    *
    * @param thingId The Thing that owns this conversation.
    * @param text The QLText to post.
    * @param responseTo The ID of the comment to respond to.
    * @return The created node.
    */
  def addComment(thingId: TID, text: String, responseTo: CommentId): TestOp[ConvNode] =
    TestOp.client { _[ConversationFunctions].addComment(thingId, text, Some(responseTo)).call() }

  def addComment(thingId: TID, text: String, responseTo: ConvNode): TestOp[ConvNode] =
    addComment(thingId, text, responseTo.comment.id)

  def deleteComment(thingId: TID, commentId: CommentId): TestOp[Unit] =
    TestOp.client { _[ConversationFunctions].deleteComment(thingId, commentId).call() }

  def deleteComment(thingId: TID, commentNode: ConvNode): TestOp[Unit] =
    deleteComment(thingId, commentNode.comment.id)

  final val commentDeletedText = "*Comment deleted*"

  def commentText(node: ConvNode): String = node.comment.content.strip.str

  def assertCommentText(node: ConvNode, text: String): TestOp[Unit] = {
    TestOp.pure { commentText(node) should equal (text) }
  }

}

object ConvMidFuncs extends ConvMidFuncs
