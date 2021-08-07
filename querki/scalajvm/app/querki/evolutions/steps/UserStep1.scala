package querki.evolutions.steps

import querki.ecology._
import querki.evolutions._

class UserStep1(implicit val ecology: Ecology) extends UserStep {
  val version = 1

  def doEvolve(info: UserInfo)(implicit conn: java.sql.Connection): Unit = {
    UserSQL(
      info.id,
      """
        CREATE TABLE {notename} (
          id int NOT NULL,
          sender bigint NOT NULL,
          toIdentity bigint DEFAULT NULL,
          ecotId smallint NOT NULL,
          noteType smallint NOT NULL,
          sentTime datetime NOT NULL,
          spaceId bigint DEFAULT NULL,
          thingId bigint DEFAULT NULL,
          props mediumtext NOT NULL,
          isRead boolean NOT NULL,
          isDeleted boolean NOT NULL,
          PRIMARY KEY (id),
          INDEX current_notes_idx_{notename} (sentTime, isDeleted)
        ) DEFAULT CHARSET=utf8
        """
    ).execute()
  }
}
