package querki.test.mid

import java.sql.Connection
import play.api.Application
import play.api.db.DBApi
import anorm._

import querki.db._
import ShardKind._
import querki.globals._
import querki.security.Encryption
import querki.system.TOSModule

class MidFuncDB {

  def setupDatabase(app: Application) = {

    // CHEAT: since Encryption is effectively static, we do a runaround here, with a fake Ecology. Note that we
    // aren't even bothering to init it, just have it to create the Encryption Ecot:
    val setupEcology: Ecology = new querki.ecology.EcologyImpl(Some(app))
    val Encryption: Encryption = new querki.security.EncryptionEcot(setupEcology)

    val dbapi = app.injector.instanceOf(classOf[DBApi])

    def DB[A](db: ShardKind.ShardKind)(trans: Connection => A): A = {
      dbapi.database(dbName(db)).withTransaction { implicit conn =>
        try {
          trans(conn)
        } catch {
          case ex: Exception => {
            QLog.error("Exception executing DB transaction during setupDatabase", ex)
            throw ex
          }
        }
      }
    }

    // Note that, in the H2 world, we don't need to create the databases, and in fact
    // aren't allowed to!

    DB(System) { implicit conn =>
      def cmd(str: String) = SQL(str).execute()

      systemDBCmds(Encryption).foreach(cmd(_))

      // Mark any pre-existing members as being up-to-date on the Terms of Service:
      cmd(s"UPDATE User SET tosVersion = ${TOSModule.currentVersion.version}")
    }

    DB(User) { implicit conn =>
      SQL("""CREATE TABLE OIDNexter (
               nextId int NOT NULL,
               shard int DEFAULT 1
             ) DEFAULT CHARSET=utf8;""").execute()
      SQL("INSERT INTO OIDNexter (shard, nextId) VALUES (2, 1)").execute()
    }
  }

  val systemPassword = "systemPassword"
  val adminPassword = "adminPassword"
  val userPassword = "userPassword"

  // Pretty closely based on the system evolutions, and should remain roughly in sync with them,
  // but slightly simplified for efficiency:
  def systemDBCmds(enc: Encryption): List[String] = List(
    """CREATE TABLE User (
      id bigint NOT NULL,
      name varchar(255) DEFAULT NULL,
      level tinyint,
      join_date datetime DEFAULT NULL,
      tosVersion int DEFAULT 0,
      userVersion int DEFAULT 0,
      lastNoteChecked int DEFAULT 0,
      PRIMARY KEY (id)
    ) DEFAULT CHARSET=utf8;""",
    "INSERT INTO User (id, name, level) VALUES (9, 'testSystem', 100);",
    "INSERT INTO User (id, name, level) VALUES (11, 'testAdmin', 10);",
    "INSERT INTO User (id, name, level) VALUES (31, 'testUser', 4);",
    """CREATE TABLE OIDNexter (
      nextId int NOT NULL,
      shard int DEFAULT 1
    ) DEFAULT CHARSET=utf8;""",
    "INSERT INTO OIDNexter (nextId) VALUES (0);",
    """CREATE TABLE Spaces (
      id bigint NOT NULL,
      shard int NOT NULL,
      name varchar(255) NOT NULL,
      display varchar(255) NOT NULL,
      owner bigint NOT NULL,
      size int NOT NULL,
      version int DEFAULT 1,
      status int DEFAULT 0,
      PRIMARY KEY (id)
    ) DEFAULT CHARSET=utf8;""",
    """CREATE TABLE Identity (
      id bigint NOT NULL,
      name varchar(255) NOT NULL,
      userId bigint DEFAULT NULL,
      kind int NOT NULL,
      provider int DEFAULT NULL,
      handle varchar(255) DEFAULT NULL,
      email varchar(255) DEFAULT NULL,
      authentication varchar(255) DEFAULT NULL,
      PRIMARY KEY (id)
    ) DEFAULT CHARSET=utf8;""",
    "CREATE INDEX identity_by_email ON Identity (email);",
    "CREATE INDEX identity_by_name ON Identity (name);",
    "CREATE INDEX spaces_by_owner ON Spaces (owner);",
    s"""INSERT INTO Identity (id, handle, name, userId, kind, email, authentication)
      VALUES (97, 'testSystem', 'Test User', 9, 2, 'testSystem@querki.net', '${enc.calcHash(systemPassword)}');""",
    s"""INSERT INTO Identity (id, handle, name, userId, kind, email, authentication)
      VALUES (98, 'testAdmin', 'Test Admin', 11, 2, 'testAdmin@querki.net', '${enc.calcHash(adminPassword)}');""",
    s"""INSERT INTO Identity (id, handle, name, userId, kind, email, authentication)
      VALUES (99, 'testUser', 'Test User', 31, 2, 'testUser@querki.net', '${enc.calcHash(userPassword)}');""",
    """CREATE TABLE SpaceMembership (
      identityId bigint NOT NULL,
      spaceId bigint NOT NULL,
      membershipState tinyint DEFAULT 0,
      lastAccess datetime DEFAULT NULL,
      nickname VARCHAR(255) DEFAULT NULL,
      PRIMARY KEY (identityId, spaceId)
    ) DEFAULT CHARSET=utf8;""",
    """CREATE TABLE Apps (
      space_id bigint NOT NULL,
      app_id bigint NOT NULL,
      app_version bigint DEFAULT 0,
      position int DEFAULT 0,
      PRIMARY KEY (space_id, app_id),
      KEY spaceKey (space_id),
      KEY appKey (app_id),
      CONSTRAINT space_rel FOREIGN KEY (space_id) REFERENCES Spaces (id) ON DELETE CASCADE,
      CONSTRAINT app_rel FOREIGN KEY (app_id) REFERENCES Spaces (id) ON DELETE CASCADE
    ) DEFAULT CHARSET=utf8;"""
  )
}
