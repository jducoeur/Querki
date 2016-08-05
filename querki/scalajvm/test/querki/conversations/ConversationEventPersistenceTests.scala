package querki.conversations

import querki.conversations.messages._
import querki.globals._
import querki.persistence._
import querki.test._

import PersistentEvents._

class ConversationEventPersistenceTests(env:PersistEnv) 
  extends PersistTest(env) with querki.types.ModelTypeDefiner with PersistentEvents with querki.identity.IdentityPersistence 
{
  
  lazy val Conversations = interface[Conversations]
  
  lazy val CommentText = Conversations.CommentText
  
  val s = env.commonSpace
  implicit val state = s.state
  val owner = s.owner.mainIdentity.id
  val m1 = s.member1.user.mainIdentity.id
  val m2 = s.member2.user.mainIdentity.id
  
  val thingId = s.instance.id

  var nextId:CommentId = 0
  
  /**
   * Helper for creating simplified conversations, for testing. Doesn't yet try to test all possible
   * flags, just one or two for sanity-checking.
   */
  def comment(
    who:OID, 
    text:String, 
    respIn:Seq[ConversationNode] = Seq.empty,
    isDeleted:Boolean = false):ConversationNode = 
  {
    val commentId = nextId
    nextId += 1
    
    // Mark each of the responses as a response to this root:
    val resps = respIn match {
      case head :: tail => {
        head.copy(comment = head.comment.copy(primaryResponse = true, responseTo = Some(commentId))) +:
        tail.map( tailResp =>
          tailResp.copy(comment = tailResp.comment.copy(responseTo = Some(commentId)))
        )
      }
      case Nil => Seq.empty
    }
    
    ConversationNode(
      Comment(
        state.id,
        commentId,
        thingId,
        who,
        None,
        Map(CommentText(text)),
        None,
        false,
        isDeleted = isDeleted
      ),
      resps
    )
  }
  
  val testConvs = ThingConversations(Seq(
    comment(owner, "Let's have us a conversation, chaps, shall we?", Seq(
      comment(m1, "Why yes, good fellow, let us do so", Seq(
        comment(m2, "Dudes, why are you talking like that?"),
        comment(owner, "Why, how should we speak, sir? Do you expect us to chat like mere ruffians?"),
        comment(m2, "Duuude, really?")
      ), isDeleted = true),
      comment(m2, "So, like, what do you want to talk about?", Seq(
        comment(owner, "Well, I'm sure I don't know. I'm just the manager here. I delegate the decisions to you.")
      ))
    ))
  ))
  
  val roundtripped = rehydrate(checkSerialization(dh(testConvs)))
  // Check that flags are roundtripping correctly:
  assert(roundtripped.comments(0).responses(0).comment.isDeleted == true)
  assert(roundtripped.comments(0).responses(1).comment.isDeleted == false)
  
  // Also, check the central events:
  val simpleComment = comment(owner, "Why, hello there!").comment
  checkSerialization(DHAddComment(dh(simpleComment)))
  checkSerialization(DHDeleteComment(s.owner, simpleComment.id))
}
