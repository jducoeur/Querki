package controllers

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
import querki.spaces.messages._
import querki.time.DateTime
import querki.util._

class ConversationController extends ApplicationBase {

  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val Core = interface[querki.core.Core]
  lazy val Person = interface[querki.identity.Person]
  
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
    
  def comment2Display(c:Comment):CommentDisplay = {
    CommentDisplay(
      c.id,
      // TODO: No! Horrible! Inefficient! This must be replaced!
      UserAccess.getIdentity(c.authorId).map(_.name).getOrElse("Unknown"),
      Conversations.CommentText.firstOpt(c.props).map(_.text).map(Wikitext(_).display.toString).getOrElse(""),
      c.primaryResponse,
      c.createTime)
  }
  
  def node2Display(n:ConversationNode):NodeDisplay = {
    NodeDisplay(
      comment2Display(n.comment),
      n.responses.map(node2Display(_)))
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
        "createTime" -> c.createTime
      )
    }
  }
  
  implicit val nodeWrites:Writes[NodeDisplay] = new Writes[NodeDisplay] {
    def writes(n:NodeDisplay):JsValue = {
      Json.obj(
        "comment" -> n.comment,
        "responses" -> Json.arr(n.responses.map(Json.toJson(_)))
      )
    }
  }
  
  implicit val addedNodeWrites = new Writes[AddedNode] {
    def writes(msg:AddedNode):JsValue = {
      Json.obj(
        "parentId" -> msg.parentId,
        "node" -> node2Display(msg.node)
      )
    }
  }
  
  def getConversations(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId) { implicit rc =>
    val msg = ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id))
    askSpace(msg) {
      case ThingConversations(convs) => {
        val convJson = Json.toJson(convs.map(node2Display(_)))
        Ok(convJson)
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
        Ok(Json.toJson(reply))
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