package querki.conversations

import models.ModelPersistence
import models.ModelPersistence._
import querki.conversations.messages._
import querki.globals._
import querki.identity.IdentityPersistence._
import querki.persistence._
import querki.time.DateTime

object PersistentEvents {

  /**
   * The dehydrated, persistable version of a Comment.
   */
  case class DHComment(
    @KryoTag(1) id: CommentId,
    @KryoTag(2) authorId: OID,
    @KryoTag(3) authorizedBy: Option[OID],
    @KryoTag(4) props: DHPropMap,
    @KryoTag(5) responseTo: Option[CommentId],
    // This is a packed array of the boolean flags for this comment:
    @KryoTag(6) flags: Byte,
    @KryoTag(7) createTime: DateTime
  ) extends UseKryo

  case class DHNode(
    @KryoTag(1) comment: DHComment,
    @KryoTag(2) responses: List[DHNode]
  ) extends UseKryo

  /**
   * The dehydrated version of ThingConversations.
   */
  case class DHConvs(@KryoTag(1) convs: List[DHNode]) extends UseKryo

  /**
   * The event that a comment has been added. Note that the "requester" is implicitly the
   * author in this case.
   */
  case class DHAddComment(
    @KryoTag(1) comment: DHComment
  ) extends UseKryo

  /**
   * Event generated when a comment has been deleted.
   */
  case class DHDeleteComment(
    @KryoTag(1) req: UserRef,
    @KryoTag(2) commentId: CommentId
  ) extends UseKryo
}

trait PersistentEvents extends ModelPersistence { self: EcologyMember with querki.types.ModelTypeDefiner =>

  import PersistentEvents._

  /**
   * The OID of the Thing that these Comments are for.
   */
  def thingId: OID

  // Names for the bit-shifts for each flag:
  final val FPrimary = 0
  final val FModeration = 1
  final val FEdited = 2
  final val FDeleted = 3
  final val FArchived = 4

  def dhFlags(comment: Comment): Byte = {
    implicit def bool2n(b: Boolean): Int = if (b) 1 else 0

    ((comment.primaryResponse << FPrimary) |
      (comment.needsModeration << FModeration) |
      (comment.isEdited << FEdited) |
      (comment.isDeleted << FDeleted) |
      (comment.isArchived << FArchived)).toByte
  }

  def dh(comment: Comment)(implicit state: SpaceState): DHComment = {
    DHComment(
      comment.id,
      comment.authorId,
      comment.authorizedBy,
      comment.props,
      comment.responseTo,
      dhFlags(comment),
      comment.createTime
    )
  }

  def rehydrate(dh: DHComment)(implicit state: SpaceState): Comment = {
    implicit def n2bool(n: Int): Boolean = if (n == 1) true else false
    def flag(n: Int): Boolean = (dh.flags >> n) & 0x1

    Comment(
      state.id,
      dh.id,
      thingId,
      dh.authorId,
      dh.authorizedBy,
      dh.props,
      dh.responseTo,
      flag(FPrimary),
      dh.createTime,
      flag(FModeration),
      flag(FEdited),
      flag(FDeleted),
      flag(FArchived)
    )
  }

  def dh(node: ConversationNode)(implicit state: SpaceState): DHNode = {
    DHNode(
      dh(node.comment),
      node.responses.toList.map(dh(_))
    )
  }

  def rehydrate(dh: DHNode)(implicit state: SpaceState): ConversationNode = {
    ConversationNode(
      rehydrate(dh.comment),
      dh.responses.map(rehydrate(_))
    )
  }

  def dh(convs: ThingConversations)(implicit state: SpaceState): DHConvs = {
    DHConvs(convs.comments.toList.map(dh(_)))
  }

  def rehydrate(dh: DHConvs)(implicit state: SpaceState): ThingConversations = {
    ThingConversations(dh.convs.map(rehydrate(_)))
  }
}
