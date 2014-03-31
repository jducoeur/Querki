package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.concurrent.Promise
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._

import models.{OID, UnknownOID, Wikitext}

import querki.conversations.messages._
import querki.identity.PublicIdentity
import querki.spaces.messages._
import querki.time.DateTime
import querki.util._

class ConversationController extends ApplicationBase {

  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val Core = interface[querki.core.Core]
  lazy val Person = interface[querki.identity.Person]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  /**
   * What we actually send to the client.
   */
  case class CommentDisplay(
    id:CommentId,
    author:String,
    html:String,
    primaryResponse:Boolean,
    createTime:DateTime)
    
  case class NodeDisplay(
    comment:CommentDisplay,
    responses:Seq[NodeDisplay])
    
  case class AddedNodeDisplay(
    parentId:Option[CommentId],
    node:NodeDisplay)
    
  def comment2Display(c:Comment)(implicit ids:Map[OID, PublicIdentity]):CommentDisplay = {
    CommentDisplay(
      c.id,
      ids.get(c.authorId).map(_.name).getOrElse("Unknown"),
      Conversations.CommentText.firstOpt(c.props).map(_.text).map(Wikitext(_).display.toString).getOrElse(""),
      c.primaryResponse,
      c.createTime)
  }
  
  def node2Display(n:ConversationNode)(implicit ids:Map[OID, PublicIdentity]):NodeDisplay = {
    NodeDisplay(
      comment2Display(n.comment),
      n.responses.map(node2Display(_)))
  }
  
  def addedNode2Display(a:AddedNode)(implicit ids:Map[OID, PublicIdentity]):AddedNodeDisplay = {
    AddedNodeDisplay(a.parentId, node2Display(a.node))
  }
  
  /**
   * The wire representation of a Comment, which is generated from the real thing.
   */
  implicit val commentWrites = new Writes[CommentDisplay] {
    def writes(c:CommentDisplay):JsValue = {
      Json.obj(
        "id" -> c.id,
        "author" -> c.author,
        "html" -> c.html,
        "primary" -> c.primaryResponse,
        "createTime" -> c.createTime
      )
    }
  }
  
  implicit val nodeWrites:Writes[NodeDisplay] = new Writes[NodeDisplay] {
    def writes(n:NodeDisplay):JsValue = {
      Json.obj(
        "comment" -> n.comment,
        "responses" -> n.responses
      )
    }
  }
  
  implicit val addedNodeWrites = new Writes[AddedNodeDisplay] {
    def writes(msg:AddedNodeDisplay):JsValue = {
      Json.obj(
        "parentId" -> msg.parentId,
        "node" -> msg.node
      )
    }
  }
  
  def getIds(node:ConversationNode):Set[OID] = {
    getIds(node.responses) + node.comment.authorId
  }
  
  def getIds(nodes:Seq[ConversationNode]):Set[OID] = {
    (Set.empty[OID] /: nodes) { (set, node) => set ++ getIds(node) }
  }
  
  def getIdentities(nodes:Seq[ConversationNode]):Future[Map[OID, PublicIdentity]] = {
    val ids = getIds(nodes)
    IdentityAccess.getIdentities(ids.toSeq)
  }
  
  def getConversations(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId) { implicit rc =>
    val msg = ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id))
    askSpace(msg) {
      case ThingConversations(convs) => {
        Async {
          // TODO: cope with errors returned from here!
          getIdentities(convs).map { implicit identityMap =>
            val convJson = Json.toJson(convs.map(node2Display(_)))
            Ok(convJson)
          }
        }
      }
      case ThingError(ex, stateOpt) => {
        BadRequest(ex.display(Some(rc)))
      }
    }
  }
  
  def addComment(ownerId:String, spaceId:String, thingId:String, text:String, responseToStr:String) = withThing(false, ownerId, spaceId, thingId) { implicit rc =>
    implicit val state = rc.state.get
    val responseTo = {
      if (responseToStr.length() > 0)
        Some(java.lang.Integer.parseInt(responseToStr))
      else
        None
    }
    val comment = Comment(
        state.id,
        UnknownCommentId,
        rc.thing.get.id,
        // TODO: we need a better concept of "my current identity in this Space"!
        rc.localIdentity.map(_.id).getOrElse(UnknownOID),
        None,
        Core.toProps(
          Conversations.CommentText(text))(),
        responseTo,
        true     // TODO: primaryResponse
    )
    val msg = ConversationRequest(rc.requesterOrAnon, rc.ownerId, state.id, NewComment(comment))
    askSpace(msg) {
      case reply @ AddedNode(parentId, node) => {
        Async {
          // TODO: cope with errors returned from here!
          getIdentities(Seq(node)).map { implicit identityMap =>
            Ok(Json.toJson(addedNode2Display(reply)))      
          }
        }
      }
      case ThingError(ex, stateOpt) => {
        BadRequest(ex.display(Some(rc)))
      }
    }
  }
  
  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("convJsRoutes")(
        routes.javascript.ConversationController.getConversations,
        routes.javascript.ConversationController.addComment
      )
    ).as("text/javascript")
  }

}