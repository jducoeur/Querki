package querki.evolutions.steps

import java.sql.Connection

import querki.ecology._

import querki.evolutions._

class Step4(implicit val ecology:Ecology) extends Step {
  val version = 4
  
  def doEvolve(info:SpaceInfo)(implicit conn:java.sql.Connection):Unit = {
    SpaceSQL(info.id, """
        |CREATE TABLE {cname} (
        |  id int NOT NULL,
        |  thingId bigint NOT NULL,
        |  authorId bigint NOT NULL,
        |  authorizedBy bigInt DEFAULT NULL,
        |  props MEDIUMTEXT NOT NULL,
        |  createTime datetime NOT NULL,
        |  responseTo int DEFAULT NULL,
        |  needsModeration tinyint NOT NULL DEFAULT 0,
        |  primaryResponse tinyint NOT NULL,
        |  isEdited tinyint NOT NULL DEFAULT 0,
        |  isDeleted tinyint NOT NULL DEFAULT 0,
        |  isArchived tinyint NOT NULL DEFAULT 0,
        |  PRIMARY KEY (id),
        |  INDEX comments_by_thingid (thingId),
        |  INDEX moderation_queue (needsModeration, createTime)
        |);
        |""".stripMargin).execute
  }
}