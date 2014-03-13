package querki.conversations

import akka.actor._
import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

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
    case LoadCommentsFor(thingId, state) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        val commentStream = SpaceSQL("""
	          SELECT * FROM {cname} 
               WHERE thingId = {thingId}
	          """).on("thingId" -> thingId.raw)()
	          
	    val comments = commentStream.map { row =>
          Comment(
              spaceId,
              row.int("id"),
              row.oid("thingId"),
              row.oid("authorId"),
              row.optOid("authorizedBy"),
              SpacePersistence.deserializeProps(row.string("props"), state),
              row.dateTime("createTime"),
              row.opt[Int]("responseTo"),
              row.bool("needsModeration"),
              row.bool("primaryResponse"),
              row.bool("isEdited"),
              row.bool("isDeleted"),
              row.bool("isArchived")
            )
        }
        
        sender ! AllCommentsFor(thingId, comments.force)
      }
    }
  }
}