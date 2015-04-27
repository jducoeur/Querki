package querki.conversations

import scala.concurrent.{Future, Promise}

import akka.actor._

import models.{UnknownOID, Wikitext}

import querki.globals._

import querki.data.TID
import querki.identity.PublicIdentity
import querki.identity.IdentityCacheMessages._
import querki.session.{AutowireApiImpl, AutowireParams}
import querki.spaces.messages.{ConversationRequest, ThingError}
import querki.util.Requester
import Requester._
import querki.values.RequestContext

import messages._

/**
 * Implements the Client API for Conversations.
 * 
 * Note that this trait is specifically design to be mixed into the UserSession.
 */
class ConversationFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with ConversationFunctions {

  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val Core = interface[querki.core.Core]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
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
  
  def getConversationsFor(thingId:TID):Future[ConversationInfo] = withThing(thingId) { thing =>
    val canComment = rc.localIdentity.map(identity => Conversations.canWriteComments(identity.id, thing, state)).getOrElse(false)
	val canReadComments = Conversations.canReadComments(user, thing, state)
		    
	if (canReadComments) {
	    // We need to store away the RC for this request, to have it available when the requests come back:
	    // TODO: *Sigh*. We'd really like to have something like Spores in Request, to catch bugs like this in
	    // the compiler...
	    implicit val theRc = rc
	    
	    val dummy:Future[ThingConversations] = for {
	      t:ThingConversations <- spaceRouter.requestForFuture[ThingConversations,ConversationInfo](ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id)))
	    }
	      yield t

//	    for {
//	      ThingConversations(convs) <- spaceRouter.requestForFuture[ThingConversations,ConversationInfo](ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id)))
//	      ids <- IdentityAccess.identityCache.requestFor[IdentitiesFound](GetIdentities(getIds(convs).toSeq))
//	      IdentitiesFound(identities) = ids
//          apiConvs = convs.map(toApi(_)(identities, theRc))
//	    }
//	      yield ConversationInfo(canComment, canReadComments, apiConvs)
	      
//	    spaceRouter.requestForFuture[ThingConversations, ConversationInfo](
//	        ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id))).flatMap 
//	    { tcs:ThingConversations =>
//	      val ThingConversations(convs) = tcs
//	      IdentityAccess.identityCache.requestFor[IdentitiesFound](GetIdentities(getIds(convs).toSeq)).map { ids =>
//	        val IdentitiesFound(identities) = ids
//	        val apiConvs = convs.map(toApi(_)(identities, theRc))
//	        ConversationInfo(canComment, canReadComments, apiConvs)
//	      }
//	    }
//	    
//	    spaceRouter.requestFuture[ConversationInfo](ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id))).flatMap {
//	      case (ThingConversations(convs), promise) => {
//	        // TODO: the IdentityCache is a bit of a bottleneck. Should we have a temp cache locally here?
//	        IdentityAccess.identityCache.request(GetIdentities(getIds(convs).toSeq)).map {
//		      case IdentitiesFound(identities) => {
//		        // Okay, we now have all the necessary info, so build the API version of the conversations.
//		        implicit val ids = identities
//		        val apiConvs = convs.map(toApi(_))
//		        
//		    	ConversationInfo(canComment, canReadComments, apiConvs)
//		      }
//		      case _ => throw new Exception("Unable to find Conversation identities!")
//	        }
//	      }
//	      case (ThingError(ex, stateOpt), promise) => throw ex
//	    }
	} else {
	  // The requester can't read the comments, so don't bother collecting them:
	  Future.successful(ConversationInfo(canComment, canReadComments, Seq.empty))
	}
  }

  def addComment(thingId:TID, text:String, responseTo:Option[CommentId]):Future[ConvNode] = withThing(thingId) { thing =>
//    val promise = Promise[ConvNode]
    
    // TODO: we need a better concept of "my current identity in this Space"!
    val authorId = rc.localIdentity.map(_.id).getOrElse(UnknownOID)
    
    val comment = Comment(
        state.id,
        UnknownCommentId,
        thing.id,
        authorId,
        None,
        Core.toProps(
          Conversations.CommentText(text))(),
        responseTo,
        true     // TODO: primaryResponse
    )
    
    val theRc = rc
    
    val dummy = for {
      reply @ AddedNode(parentId, node) <- spaceRouter.requestFuture[ConvNode](ConversationRequest(user, rc.ownerId, state.id, NewComment(comment)))
      IdentityFound(identity) <- IdentityAccess.identityCache.request(GetIdentityRequest(authorId))
    }
      yield toApi(node)(Map(authorId -> identity), theRc)
    
    spaceRouter.requestFuture[ConvNode](ConversationRequest(user, rc.ownerId, state.id, NewComment(comment))).flatMap {
      case reply @ AddedNode(parentId, node) => {
        IdentityAccess.identityCache.request(GetIdentityRequest(authorId)).map {
	      case IdentityFound(identity) => {
	    	toApi(node)(Map(authorId -> identity), theRc)
//	    	promise.success(toApi(node)(Map(authorId -> identity), theRc))
	      }
//	      case _ => promise.failure(new Exception("Unable to find Conversation identities!"))
        }

      }
//      case ThingError(ex, stateOpt) => promise.failure(ex)
    }

//    promise.future
  }
  
  def deleteComment(thingId:TID, commentId:CommentId):Future[Unit] = withThing(thingId) { thing =>
    spaceRouter.requestFuture[Unit](ConversationRequest(user, rc.ownerId, state.id, DeleteComment(thing.id, commentId))).map {
      case CommentDeleted => ()
      case CommentNotDeleted => throw new Exception("Unable to delete comment")      
    }
  }
}
