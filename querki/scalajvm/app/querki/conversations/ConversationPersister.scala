package querki.conversations

import akka.actor._
import anorm.{Success=>AnormSuccess,_}
import anorm.SqlParser._
import play.api.db._

import models.OID

import querki.db._
import querki.db.ShardKind._
import querki.ecology._
import querki.time.TimeAnorm._
import querki.util.SqlHelpers._

import messages._
import PersistMessages._

/**
 * This Actor manages all of the database interactions for the Conversations of a single Space. It is broken out
 * of the SpaceConversationActor for robustness, testability, and separation of latency issues.
 * 
 * TODO: in principle, this Actor (as well as the other Persisters) should be broken out into a separate Dispatcher,
 * so that they can't clog up all the system threads.
 */
private[conversations] class ConversationPersister(val spaceId:OID, implicit val ecology:Ecology) extends Actor with EcologyMember {
  
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  
  def SpaceSQL(query:String):SqlQuery = SpacePersistence.SpaceSQL(spaceId, query)
  
  def receive = {
    case GetMaxCommentId => {
      QDB(ShardKind.User) { implicit conn =>
        val nOpt = 
          SpaceSQL("""SELECT MAX(id) as max from {cname}""")
            .as(int("max").?.single)
        sender ! CurrentMaxCommentId(nOpt.getOrElse(0))
      }
    }
    
    
    case LoadCommentsFor(thingId, state) => {
      val commentParser:RowParser[Comment] =
        for {
          commentId <- int("id")
          thingId <- oid("thingId")
          authorId <- oid("authorId")
          authBy <- oid("authorizedBy").?
          props <- str("props")
          resp <- int("responseTo").?
          primary <- bool("primaryResponse")
          created <- dateTime("createTime")
          needsMod <- bool("needsModeration")
          edited <- bool("isEdited")
          deleted <- bool("isDeleted")
          archived <- bool("isArchived")
        }
          yield
            Comment(
              spaceId, commentId, thingId, authorId, authBy,
              SpacePersistence.deserializeProps(props, state),
              resp, primary, created, needsMod,
              edited, deleted, archived
            )
        
      QDB(ShardKind.User) { implicit conn =>
        // TODO: this is causing warnings, because of the deprecated apply. But I'm leaving it
        // in place for now, because Conversations should move to Akka Persistence before too long.
        val comments = SpaceSQL("""
	          SELECT * FROM {cname} 
               WHERE thingId = {thingId}
	          """)
	        .on("thingId" -> thingId.raw)
	        .as(commentParser.*)
	          
        // TBD: this may be conceptually inappropriate. If we want to think in EventSourcedProcessor terms, we should probably
        // instead send a stream of AddComment messages, I think.
        sender ! AllCommentsFor(thingId, comments)
      }
    }
    
    case AddComment(comment, state) => {
      QDB(ShardKind.User) { implicit conn =>
        // Note that a bunch of the Booleans simply default to false, and are irrelevant for a new Comment:
        SpaceSQL("""
            INSERT INTO {cname}
            ( id,   thingId,   authorId,   authorizedBy,   props,   createTime,   responseTo,   needsModeration,   primaryResponse) VALUES
            ({id}, {thingId}, {authorId}, {authorizedBy}, {props}, {createTime}, {responseTo}, {needsModeration}, {primaryResponse})
            """).on(
                "id" -> comment.id,
                "thingId" -> comment.thingId.raw,
                "authorId" -> comment.authorId.raw,
                "authorizedBy" -> comment.authorizedBy.map(_.id.raw),
                "props" -> SpacePersistence.serializeProps(comment.props, state),
                "createTime" -> comment.createTime,
                "responseTo" -> comment.responseTo,
                "needsModeration" -> comment.needsModeration,
                "primaryResponse" -> comment.primaryResponse).executeUpdate
      }
    }
    
    case UpdateComment(comment, state) => {
      // Note that we tacitly assume that many fields are immutable; we only update the ones that can change:
      QDB(ShardKind.User) { implicit conn =>
        SpaceSQL("""
            UPDATE {cname}
               SET authorizedBy = {authorizedBy}, props = {props}, needsModeration = {needsModeration}, primaryResponse = {primaryResponse},
                   isEdited = {isEdited}, isDeleted = {isDeleted}, isArchived = {isArchived} 
             WHERE id = {id}
            """).on(
                "id" -> comment.id,
                "authorizedBy" -> comment.authorizedBy.map(_.id.raw),
                "props" -> SpacePersistence.serializeProps(comment.props, state),
                "needsModeration" -> comment.needsModeration,
                "primaryResponse" -> comment.primaryResponse,
                "isEdited" -> comment.isEdited,
                "isDeleted" -> comment.isDeleted,
                "isArchived" -> comment.isArchived).executeUpdate
      }
    }
  }
}