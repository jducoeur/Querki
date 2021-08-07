package querki.conversations

/**
 * Public API for Conversations. These are mainly Actor-centric, so we focus on the messages.
 */
package object messages {

  /**
   * The ID of a single Comment. Note that this is scoped by the Space!
   */
  type CommentId = Int

  /**
   * The Unknown value for CommentId. (Duh.)
   */
  val UnknownCommentId = -1

}
