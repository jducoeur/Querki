package querki.conversations

import models.OID

import querki.conversations.messages.Comment
import querki.values.SpaceState

private[conversations] object PersistMessages {
  /**
   * Request to load all of the Comments for the specified Thing.
   */
  case class LoadCommentsFor(thingId:OID, state:SpaceState)
  
  /**
   * Response to LoadCommentsFor().
   */
  case class AllCommentsFor(thingId:OID, comments:Seq[Comment])
  
  /**
   * Store a single Comment.
   */
  case class AddComment(comment:Comment, state:SpaceState)
}