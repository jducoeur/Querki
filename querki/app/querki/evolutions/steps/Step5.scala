package querki.evolutions.steps

import java.sql.Connection

import querki.ecology._

import querki.evolutions._

class Step5(implicit val ecology:Ecology) extends Step {
  val version = 5
  
  def doEvolve(info:SpaceInfo)(implicit conn:java.sql.Connection):Unit = {
    SpaceSQL(info.id, """
        |CREATE TABLE {uvname} (
        |  thingId bigint NOT NULL,
        |  propertyId bigint NOT NULL,
        |  userId bigint NOT NULL,
        |  propValue MEDIUMTEXT NOT NULL,
        |  modTime datetime NOT NULL,
        |  PRIMARY KEY (thingId, propertyId, userId)
        |);
        |""".stripMargin).execute
  }
}