package querki.conversations

import scala.concurrent.{Future, Promise}

import akka.actor._

import models.{UnknownOID, Wikitext}

import querki.api.{SpaceApiImpl, AutowireParams}
import querki.data.TID
import querki.globals._
import querki.identity.PublicIdentity
import querki.identity.IdentityCacheMessages._
import querki.spaces.messages.{ConversationRequest, ThingError}
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
  
  def convTrace = Conversations.convTrace _
  
  def doRoute(req:Request):Future[String] = route[ConversationFunctions](this)(req)
  
  def getIds(node:ConversationNode):Set[OID] = {
    getIds(node.responses) + node.comment.authorId
  }
  
  def getIds(nodes:Seq[ConversationNode]):Set[OID] = {
    (Set.empty[OID] /: nodes) { (set, node) => set ++ getIds(node) }
  }
  
  def toApi(c:Comment)(implicit identities:Map[OID, PublicIdentity], theRc:RequestContext):CommentInfo = {
    CommentInfo(
      c.id,
      ClientApi.identityInfo(identities(c.authorId)),
      if (c.isDeleted)
        Wikitext("*Comment deleted*")
      else
        Wikitext(Conversations.CommentText.firstOpt(c.props).map(_.text).getOrElse("")),
      c.primaryResponse,
      c.createTime.getMillis,
      theRc.isOwner || theRc.requesterOrAnon.hasIdentity(c.authorId),
      c.isDeleted
    )
  }
  
  def toApi(node:ConversationNode)(implicit identities:Map[OID, PublicIdentity], theRc:RequestContext):ConvNode = {
    ConvNode(toApi(node.comment), node.responses.map(toApi(_)))
  }
  
  def localIdentity = Person.localIdentities(user)(state).headOption
  
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
        ThingConversations(convs) <- spaceRouter.requestFor[ThingConversations](ConversationRequest(rc.requesterOrAnon, state.id, GetConversations(thing.id)))
        identities <- IdentityAccess.getIdentities(getIds(convs).toSeq)
        apiConvs = convs.map(toApi(_)(identities, theRc))
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
      AddedNode(parentId, node) <- spaceRouter.request(ConversationRequest(user, state.id, NewComment(comment)))
      dummy1 = convTrace(s"    Have added the node for the new comment")
      IdentityFound(identity) <- IdentityAccess.identityCache.request(GetIdentityRequest(authorId))
      dummy2 = convTrace(s"    Have found the Identity of the comment's author")
    }
      yield toApi(node)(Map(authorId -> identity), theRc)
  }
  
  def deleteComment(thingId:TID, commentId:CommentId):Future[Unit] = withThing(thingId) { thing =>
	  spaceRouter.request(ConversationRequest(user, state.id, DeleteComment(thing.id, commentId))).map {
	    case CommentDeleted => ()
	    case CommentNotDeleted => throw new Exception("Unable to delete comment")      
	  }
  }
}
