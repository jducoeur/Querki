package querki.conversations

import scala.concurrent.{Future, Promise}

import akka.actor._

import models.{Thing, UnknownOID, Wikitext}

import querki.api.{SpaceApiImpl, AutowireParams}
import querki.data.TID
import querki.globals._
import querki.identity.PublicIdentity
import querki.identity.IdentityCacheMessages._
import querki.spaces.messages.{SpaceSubsystemRequest, ThingError}
import querki.values.RequestContext

import messages._

/**
 * Implements the Client API for Conversations.
 * 
 * Note that this trait is specifically design to be mixed into the UserSession.
 */
class ConversationFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with ConversationFunctions {

  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val Core = interface[querki.core.Core]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val QL = interface[querki.ql.QL]
  
  def convTrace = Conversations.convTrace _
  
  def doRoute(req:Request):Future[String] = route[ConversationFunctions](this)(req)
  
  def getIds(node:ConversationNode):Set[OID] = {
    getIds(node.responses) + node.comment.authorId
  }
  
  def getIds(nodes:Seq[ConversationNode]):Set[OID] = {
    (Set.empty[OID] /: nodes) { (set, node) => set ++ getIds(node) }
  }
  
  def toApi(t:Thing, c:Comment)(implicit identities:Map[OID, PublicIdentity], theRc:RequestContext):Future[CommentInfo] = {
    implicit val s = state
    
    val contentFut =
      if (c.isDeleted)
        fut(Wikitext("*Comment deleted*"))
      else {
        Conversations.CommentText.firstOpt(c.props) match {
          case Some(comment) => {
            QL.process(comment, t.thisAsContext)
          }
          case _ => fut(Wikitext(""))
        }
      }
    
    contentFut.map { content =>
      CommentInfo(
        c.id,
        ClientApi.identityInfo(identities(c.authorId)),
        content,
        c.primaryResponse,
        c.createTime.getMillis,
        theRc.isOwner || theRc.requesterOrAnon.hasIdentity(c.authorId),
        c.isDeleted
      )
    }
  }
  
  def toApi(t:Thing, node:ConversationNode)(implicit identities:Map[OID, PublicIdentity], theRc:RequestContext):Future[ConvNode] = {
    for {
      thisComment <- toApi(t, node.comment)
      rest <- Future.sequence(node.responses.map(toApi(t, _)))
    }
      yield ConvNode(thisComment, rest)
  }
  
  def localIdentity = Person.localIdentities(user)(state).headOption

  /**
   * If a thread is completely deleted, just ignore it entirely, so it doesn't clog up the UI.
   * 
   * It isn't clear that this is the long-term approach to this problem, but we need the user
   * to be able to clear out bad threads from the UI.
   */
  def filterDeadThreads(convs:Seq[ConversationNode]):Seq[ConversationNode] = {
    (Seq.empty[ConversationNode] /: convs) { (seq, conv) =>
      if (conv.comment.isDeleted && filterDeadThreads(conv.responses).isEmpty)
        // This node is kaput -- move on
        seq
      else
        // Recurse into the replies
        seq :+ conv.copy(responses = filterDeadThreads(conv.responses))
    }
  }
  
  def getConversationsFor(thingId:TID):Future[ConversationInfo] = withThing(thingId) { thing =>
    implicit val s = state
    val canComment = localIdentity.map(identity => Conversations.canWriteComments(identity.id, thing, state)).getOrElse(false)
	  val canReadComments = Conversations.canReadComments(user, thing, state)
		    
  	if (canReadComments) {
	    // We need to store away the RC for this request, to have it available when the requests come back:
	    // TODO: *Sigh*. We'd really like to have something like Spores in Request, to catch bugs like this in
	    // the compiler...
	    implicit val theRc = rc
	    
	    for {
        ThingConversations(convs) <- spaceRouter.requestFor[ThingConversations](SpaceSubsystemRequest(rc.requesterOrAnon, state.id, GetConversations(thing.id)))
        identities <- IdentityAccess.getIdentities(getIds(convs).toSeq)
        apiConvs <- Future.sequence(filterDeadThreads(convs).map(toApi(thing, _)(identities, theRc)))
      }
        yield ConversationInfo(canComment, canReadComments, apiConvs)
	  } else {
	    // The requester can't read the comments, so don't bother collecting them:
	    Future.successful(ConversationInfo(canComment, canReadComments, Seq.empty))
	  }
  }

  def addComment(thingId:TID, text:String, responseTo:Option[CommentId]):Future[ConvNode] = withThing(thingId) { thing =>
    convTrace(s"    Trying to add comment $text")
    
    implicit val s = state
    val authorId = localIdentity.map(_.id).getOrElse(UnknownOID)
    
    val comment = Comment(
        state.id,
        UnknownCommentId,
        thing.id,
        authorId,
        None,
        Core.toProps(
          Conversations.CommentText(text)),
        responseTo,
        true     // TODO: primaryResponse
    )
    
    convTrace(s"    About to actually add the node")
    
    val theRc = rc
    for {
      AddedNode(parentId, node) <- spaceRouter.request(SpaceSubsystemRequest(user, state.id, NewComment(comment)))
      dummy1 = convTrace(s"    Have added the node for the new comment")
      IdentityFound(identity) <- IdentityAccess.identityCache.request(GetIdentityRequest(authorId))
      dummy2 = convTrace(s"    Have found the Identity of the comment's author")
      result <- toApi(thing, node)(Map(authorId -> identity), theRc)
    }
      yield result
  }
  
  def deleteComment(thingId:TID, commentId:CommentId):Future[Unit] = withThing(thingId) { thing =>
	  spaceRouter.request(SpaceSubsystemRequest(user, state.id, DeleteComment(thing.id, commentId))).map {
	    case CommentDeleted => ()
	    case CommentNotDeleted => throw new Exception("Unable to delete comment")      
	  }
  }
}
