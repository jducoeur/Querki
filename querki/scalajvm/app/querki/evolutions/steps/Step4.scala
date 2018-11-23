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
        |  needsModeration BOOLEAN NOT NULL DEFAULT FALSE,
        |  primaryResponse BOOLEAN NOT NULL,
        |  isEdited BOOLEAN NOT NULL DEFAULT FALSE,
        |  isDeleted BOOLEAN NOT NULL DEFAULT FALSE,
        |  isArchived BOOLEAN NOT NULL DEFAULT FALSE,
        |  PRIMARY KEY (id),
        |  INDEX comments_by_thingid_{cname} (thingId),
        |  INDEX moderation_queue_{cname} (needsModeration, createTime)
        |) DEFAULT CHARSET=utf8;
        |""".stripMargin).execute
  }
}
