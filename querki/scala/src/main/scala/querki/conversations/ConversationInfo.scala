package querki.conversations

import messages._

import models.Wikitext

import querki.data.IdentityInfo
import querki.time.Common.Timestamp

/**
 * The information about the Conversations for a Thing, as seen by this User.
 */
case class ConversationInfo(
  canComment:Boolean,
  canReadComments:Boolean,
  convs:Seq[ConvNode]
)

/**
 * One node in a Conversation Tree. Each Conversation is tree-structured; each Comment may have any number of responses
 * under it. The "primary" response (usually but not necessarily the first) is displayed differently, directly underneath.
 */
case class ConvNode(
  comment:CommentInfo,
  responses:Seq[ConvNode]
)

/**
 * The API description of a single Comment.
 */
case class CommentInfo(
  id:CommentId,
  author:IdentityInfo,
  content:Wikitext,
  primaryResponse:Boolean,
  createTime:Timestamp,
  canDelete:Boolean,
  isDeleted:Boolean)
